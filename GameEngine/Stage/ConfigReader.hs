{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TupleSections
  #-}
module GameEngine.Stage.ConfigReader where

import GameEngine.ConfigReader
import GameEngine.ConfigReader.Arg
import GameEngine.ConfigReader.ArgFmt
import GameEngine.ConfigReader.Config
import GameEngine.ConfigReader.ConfigFmt
import GameEngine.ConfigReader.Option
import GameEngine.ConfigReader.OptionFmt

import GameEngine.Thing.ConfigReader
import GameEngine.Tile.ConfigReader
import GameEngine.TileGrid.ConfigReader
import GameEngine.TileSet.ConfigReader

import GameEngine.AI
import GameEngine.Background
import GameEngine.Collect
import GameEngine.Counter
import GameEngine.Force
import GameEngine.HitBox
import GameEngine.Position
import GameEngine.Stage
import GameEngine.Thing
import GameEngine.Tile
import GameEngine.TileGrid
import GameEngine.TileSet
import GameEngine.Velocity

import Control.Applicative
import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Text hiding (filter,foldr,map,zip)
import Foreign.C.Types
import Linear hiding (trace)
import Linear.Affine
import SDL
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Map as Map

import Debug.Trace

stageConfigFmt :: ConfigFmt
stageConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "gravity"     [SomeArgFmt ArgFmtFloat
                                           ,SomeArgFmt ArgFmtFloat
                                           ])
                  (OptionFmt "gravityless" [])
                  ,DefaultFmt True         [SomeArg (ArgFloat 0)
                                           ,SomeArg (ArgFloat 1)
                                           ])

  ,(OptionPairFmt (OptionFmt "tileset"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyTileset" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "unitSize"      [SomeArgFmt ArgFmtFloat])
                  (OptionFmt "emptyUnitSize" [])
                  ,DefaultFmt True              [SomeArg $ ArgFloat 64])

  ,(OptionPairFmt (OptionFmt "baseThings"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyBaseThings" [])
                  ,DefaultFmt False            [])

  ,(OptionPairFmt (OptionFmt "subjectSpeedLimit"      [SomeArgFmt ArgFmtFloat, SomeArgFmt ArgFmtFloat])
                  (OptionFmt "emptySubjectSpeedLimit" [])
                  ,DefaultFmt False                   [])

  ,(OptionPairFmt (OptionFmt "thingSpeedLimit"      [SomeArgFmt ArgFmtFloat, SomeArgFmt ArgFmtFloat])
                  (OptionFmt "emptyThingSpeedLimit" [])
                  ,DefaultFmt False                 [])

  ,(OptionPairFmt (OptionFmt "subjectFriction"      [SomeArgFmt ArgFmtFloat])
                  (OptionFmt "emptySubjectFriction" [])
                  ,DefaultFmt False                 [])

  ,(OptionPairFmt (OptionFmt "thingFriction"      [SomeArgFmt ArgFmtFloat])
                  (OptionFmt "emptyThingFriction" [])
                  ,DefaultFmt False               [])

  ]

thingInstanceConfigFmt :: ConfigFmt
thingInstanceConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "thing"   [SomeArgFmt ArgFmtText])
                  (OptionFmt "nothing" [])
                  ,DefaultFmt False    [])

  ,(OptionPairFmt (OptionFmt "positioned"   [SomeArgFmt ArgFmtFloat
                                            ,SomeArgFmt ArgFmtFloat
                                            ])
                  (OptionFmt "unpositioned" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "moving"     [SomeArgFmt ArgFmtFloat
                                          ,SomeArgFmt ArgFmtFloat
                                          ])
                  (OptionFmt "stationary" [])
                  ,DefaultFmt False       [])

  ,(OptionPairFmt (OptionFmt "width"        [SomeArgFmt ArgFmtFloat])
                  (OptionFmt "inheritWidth" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "height"        [SomeArgFmt ArgFmtFloat])
                  (OptionFmt "inheritHeight" [])
                  ,DefaultFmt False          [])

  ,(OptionPairFmt (OptionFmt "hitbox"    [SomeArgFmt ArgFmtFloat -- x
                                         ,SomeArgFmt ArgFmtFloat -- y
                                         ,SomeArgFmt ArgFmtFloat -- width
                                         ,SomeArgFmt ArgFmtFloat -- height
                                         ])
                  (OptionFmt "emptyHitBox"  [])
                  ,DefaultFmt False      [])

  ,(OptionPairFmt (OptionFmt "maxHealth"        [SomeArgFmt ArgFmtInt])
                  (OptionFmt "defaultMaxHealth" [])
                  ,DefaultFmt False             [])

  ,(OptionPairFmt (OptionFmt "agent"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyAgent" [])
                  ,DefaultFmt False       []
                  )

  ,(OptionPairFmt (OptionFmt "contactDamage"        [SomeArgFmt ArgFmtInt])
                  (OptionFmt "defaultContactDamage" [])
                  ,DefaultFmt False                 [])

  ,(OptionPairFmt (OptionFmt "contactScore"        [SomeArgFmt ArgFmtInt])
                  (OptionFmt "defaultContactScore" [])
                  ,DefaultFmt False                [])

  ,(OptionPairFmt (OptionFmt "contactConsumed" [])
                  (OptionFmt "contactRemain"   [])
                  ,DefaultFmt False            [])

  ,(OptionPairFmt (OptionFmt "climbable"   [])
                  (OptionFmt "unclimbable" [])
                  ,DefaultFmt False        [])
  ]


type Stages = Map.Map Text Stage

-- given a path to a directory of stage directories, load all the stages
parseStages :: Collect StageAgent -> FilePath -> Renderer -> IO Stages
parseStages agents stagesPath renderer = do
  files <- listDirectory stagesPath

  -- Get the directories with a ".stage" extension
  let stageDirectories = filter ((== "stage") . extension) files

  -- Associate the NAME of all stage directories to their parsed Stage
  --mStages :: [(Text,Maybe (Stage Text))]
  mStages <- mapM (\stageDir -> (pack . name $ stageDir,) <$> parseStage agents stageDir stagesPath renderer) stageDirectories

  -- silently drop all where the stage failed to parse
  let stages :: [(Text,Stage)]
      stages = foldr (\(name,mStage) acc -> case mStage of
                                              Nothing    -> acc
                                              Just stage -> (name,stage):acc
                     )
                     []
                     mStages

  return $ Map.fromList stages

-- given a path to a stage directory, load all its dependencies and assemble the stage.
parseStage :: Collect StageAgent -> FilePath -> FilePath -> Renderer -> IO (Maybe Stage)
parseStage agents stageDir stagesPath renderer = do
  res <- parseConfigFile stageConfigFmt (stagesPath ++ "/" ++ stageDir ++ "/stage")
  case res of
    -- Failed to parse stage file
    Left _
      -> return Nothing

    Right stageConfig
      -> do let gravity           = fromArgs "gravity"           (\[SomeArg (ArgFloat x),SomeArg (ArgFloat y)] -> Force $ V2 (conv x) (conv y)) (Force $ V2 0 0) stageConfig
                unitSize          = fromArgs "unitSize"          (\[SomeArg (ArgFloat i)] -> conv i)                                            0                stageConfig
                baseThingsName    = fromArgs "baseThings"        (\[SomeArg (ArgText n)] -> n)                                                  ""               stageConfig
                tileSetName       = fromArgs "tileset"           (\[SomeArg (ArgText n)] -> n)                                                  ""               stageConfig
                subjectSpeedLimit = fromArgs "subjectSpeedLimit" (\[SomeArg (ArgFloat x),SomeArg (ArgFloat y)] -> Velocity $ V2 (conv x) (conv y)) (Velocity $ V2 100 100)     stageConfig
                thingSpeedLimit   = fromArgs "thingSpeedLimit"   (\[SomeArg (ArgFloat x),SomeArg (ArgFloat y)] -> Velocity $ V2 (conv x) (conv y)) (Velocity $ V2 100 100)     stageConfig
                subjectFriction   = fromArgs "subjectFriction"   (\[SomeArg (ArgFloat i)] -> conv i)                                            1                stageConfig
                thingFriction     = fromArgs "thingFriction"     (\[SomeArg (ArgFloat i)] -> conv i)                                            1                stageConfig

            -- Load the stages tileset
            tileset <- parseTileSet ("R/Tilesets/" ++ unpack tileSetName) renderer

            -- Get any tile aliases defined for this stage
            aliases    <- parseAliases tileset (stagesPath ++ "/" ++ stageDir)

            -- parse all the base things we might need to 'inherit' from
            baseThings <- parseThings ("R/Things/" ++ unpack baseThingsName) tileset unitSize

            -- parse all the things, and extract one named "player" to use as the subject
            things     <- parseThingInstances unitSize baseThings agents (stagesPath ++ "/" ++ stageDir)

            let mPlayer = lookupName "player" things
                them    = deleteName "player" things
                us      = collect []

            case mPlayer of
              -- No "player" tile
              Nothing
                -> return Nothing

              Just (repPlayer,_)
                -> do let player = repPlayer^.reproducing. to (`withLiveClient` _client)
                      mBackground <- parseBackground (stagesPath ++ "/" ++ stageDir) tileset aliases unitSize renderer
                      case mBackground of
                        Nothing         -> return Nothing
                        Just background -> return $ setStage background player us them gravity subjectSpeedLimit thingSpeedLimit subjectFriction thingFriction
  where
    conv :: Float -> CFloat
    conv = CFloat

parseBackground :: FilePath -> TileSet -> Aliases -> CFloat -> Renderer -> IO (Maybe Background )
parseBackground stagePath tileset aliases unitSize renderer = do
  mTexture <- loadTexture renderer (stagePath ++ "/background.bmp")
  mTileGrid <- parseTileGrid stagePath tileset aliases (floor unitSize)
  case mTileGrid of
    -- failed to parse tiles, abort!
    -- TODO: could instead create empty tiles?
    --  - Only if layout file isnt present, not if its just invalid
    Nothing
      -> return Nothing

    Just tileGrid
      -> return . mkBackground tileGrid $ Just mTexture

-- Given a set of base things which may be inherited from and a path to a directory of thing instance files,
-- parse each thing instance file and instantiate the base thing with the given configuration options.
-- Fileformat: NAME.thinginstance
parseThingInstances :: CFloat -> Collect Thing -> Collect StageAgent -> FilePath -> IO (Collect StageReproducing)
parseThingInstances unitSize baseThings agents stagePath = do
  files <- listDirectory stagePath

  -- Get the files with a ".thinginstance" extension
  let thingInstanceFiles = filter ((== "thinginstance") . extension) files

  -- Associate the NAME of all thing instnace files to their parsed things
  -- mThingInstances :: [(Text,Maybe (Reproducing Thing Pos ()))]
  mThingInstances <- mapM (\thingFile -> (pack . name $ thingFile,) <$> parseThingInstance unitSize baseThings agents thingFile stagePath) thingInstanceFiles

  -- Silently drop all where the config file failed to parse
  let thingInstances :: [(Text,StageReproducing)]
      thingInstances = foldr (\(name,mThingInstance) acc -> case mThingInstance of
                                                              Nothing            -> acc
                                                              Just thingInstance -> (name,thingInstance):acc
                             )
                             []
                             mThingInstances

  return $ mkCollect (map (\(n,t) -> (t,Name n)) thingInstances) []


-- Given a context of base Things, a thing instance file under a stage filepath,
-- parse the thing instance file and create a thing, inheriting from the base thing.
parseThingInstance :: CFloat -> Collect Thing -> Collect StageAgent -> FilePath -> FilePath -> IO (Maybe StageReproducing)
parseThingInstance unitSize baseThings agents thingInstanceFile stagePath = do
  res <- parseConfigFile thingInstanceConfigFmt (stagePath ++ "/" ++ thingInstanceFile)
  case res of
    -- Failed to parse thing instance file
    Left _
      -> return Nothing

    -- Parsed to a successful config with the given format
    Right thingInstanceConfig
      -> do -- Find the base thing it inherits from
           let mThingName = fromArgs "thing" (\[SomeArg (ArgText thingName)] -> Just . Name $ thingName) Nothing thingInstanceConfig
           case mThingName of
             -- Default to failing if no inherited thing was specified
             Nothing -> return Nothing

             Just thingName
               -> let positionOffset = fromArgs "positioned" (\[SomeArg (ArgFloat oX),SomeArg (ArgFloat oY)]
                                                               -> V2 (conv oX) (conv oY)
                                                             ) (V2 0 0) thingInstanceConfig
                      velocity       = fromArgs "moving"     (\[SomeArg (ArgFloat vX),SomeArg (ArgFloat vY)]
                                                               -> Velocity $ V2 (conv vX) (conv vY)
                                                             ) (Velocity $ V2 0 0) thingInstanceConfig
                      hitBox         = fromArgs "hitbox"     (\[SomeArg (ArgFloat x),SomeArg (ArgFloat y),SomeArg (ArgFloat w),SomeArg (ArgFloat h)]
                                                               -> HitBoxRect (Rectangle (P $ V2 (conv x) (conv y)) (V2 (conv w) (conv h)))
                                                             ) NoHitBox thingInstanceConfig

                      maxHealth      = fromArgs "maxHealth" (\[SomeArg (ArgInt h)] -> toEnum . fromEnum $ h) 50 thingInstanceConfig

                      contactDamage   = fromArgs "contactDamage" (\[SomeArg (ArgInt d)] -> fromIntegral d) 0 thingInstanceConfig
                      contactScore    = fromArgs "contactScore"  (\[SomeArg (ArgInt s)] -> fromIntegral s) 0 thingInstanceConfig
                      contactConsumed = isSet "contactConsumed" thingInstanceConfig
                      climbable       = isSet "climbable" thingInstanceConfig

                      emptyAgent :: StageAgent
                      emptyAgent = mkAgent () (\(sub,thing) () -> ("",()))
                      agent      = fromArgs "agent"     (\[SomeArg (ArgText a)] -> maybe emptyAgent fst $ lookupName (Name a) agents) emptyAgent thingInstanceConfig

                      mBaseThing = lookupName thingName baseThings
                     in -- If the base thing exists, inherit from it and modify by any config values
                       case mBaseThing of
                         -- The base thing doesnt exist. Fail!
                         Nothing
                           -> return Nothing

                         Just (baseThing,_)
                           -> do let thingWidth  = fromArgs "width"  (\[SomeArg (ArgFloat w)] -> conv w) (baseThing^.thingTile.tileWidth)  thingInstanceConfig
                                     thingHeight = fromArgs "height" (\[SomeArg (ArgFloat h)] -> conv h) (baseThing^.thingTile.tileHeight) thingInstanceConfig

                                 let thing = set thingVelocity velocity
                                           . set (thingTile.tileWidth) thingWidth
                                           . set (thingTile.tileHeight) thingHeight
                                           . set thingHitBox hitBox
                                           . set thingHealth (fromJust $ mkCounter maxHealth 0 maxHealth)
                                           . set thingContactDamage contactDamage
                                           . set thingContactScore  contactScore
                                           . set thingContactConsumed contactConsumed
                                           . set thingClimbable climbable
                                           . moveThingBy (positionOffset * V2 unitSize unitSize)
                                           $ baseThing

                                 return $ Just $ mkReproducing $ mkLive (stageClient thing) agent
  where
    conv :: Float -> CFloat
    conv = CFloat

