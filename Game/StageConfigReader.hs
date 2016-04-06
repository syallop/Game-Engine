{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TupleSections
  #-}
module Game.StageConfigReader where

import Game.ConfigReader
import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt

import Game.ThingConfigReader
import Game.TileConfigReader
import Game.TileGridConfigReader
import Game.TileSetConfigReader

import Game.Agent
import Game.Background
import Game.Force
import Game.HitBox
import Game.Stage
import Game.Thing
import Game.Tile
import Game.TileGrid
import Game.TileSet
import Game.Velocity

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
  [(OptionPairFmt (OptionFmt "gravity"     [SomeArgFmt ArgFmtInt
                                           ,SomeArgFmt ArgFmtInt
                                           ])
                  (OptionFmt "gravityless" [])
                  ,DefaultFmt True         [SomeArg (ArgInt 0)
                                           ,SomeArg (ArgInt 1)
                                           ])

  ,(OptionPairFmt (OptionFmt "tileset"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyTileset" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "unitSize"      [SomeArgFmt ArgFmtInt])
                  (OptionFmt "emptyUnitSize" [])
                  ,DefaultFmt True              [SomeArg $ ArgInt 64])

  ,(OptionPairFmt (OptionFmt "baseThings"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyBaseThings" [])
                  ,DefaultFmt False            [])

  ,(OptionPairFmt (OptionFmt "subjectSpeedLimit"      [SomeArgFmt ArgFmtInt, SomeArgFmt ArgFmtInt])
                  (OptionFmt "emptySubjectSpeedLimit" [])
                  ,DefaultFmt False                   [])

  ,(OptionPairFmt (OptionFmt "thingSpeedLimit"      [SomeArgFmt ArgFmtInt, SomeArgFmt ArgFmtInt])
                  (OptionFmt "emptyThingSpeedLimit" [])
                  ,DefaultFmt False                 [])

  ,(OptionPairFmt (OptionFmt "subjectFriction"      [SomeArgFmt ArgFmtInt])
                  (OptionFmt "emptySubjectFriction" [])
                  ,DefaultFmt False                 [])

  ,(OptionPairFmt (OptionFmt "thingFriction"      [SomeArgFmt ArgFmtInt])
                  (OptionFmt "emptyThingFriction" [])
                  ,DefaultFmt False               [])

  ]

thingInstanceConfigFmt :: ConfigFmt
thingInstanceConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "thing"   [SomeArgFmt ArgFmtText])
                  (OptionFmt "nothing" [])
                  ,DefaultFmt False    [])

  ,(OptionPairFmt (OptionFmt "positioned"   [SomeArgFmt ArgFmtInt
                                            ,SomeArgFmt ArgFmtInt
                                            ])
                  (OptionFmt "unpositioned" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "moving"     [SomeArgFmt ArgFmtInt
                                          ,SomeArgFmt ArgFmtInt
                                          ])
                  (OptionFmt "stationary" [])
                  ,DefaultFmt False       [])

  ,(OptionPairFmt (OptionFmt "width"        [SomeArgFmt ArgFmtInt])
                  (OptionFmt "inheritWidth" [])
                  ,DefaultFmt False         [])

  ,(OptionPairFmt (OptionFmt "height"        [SomeArgFmt ArgFmtInt])
                  (OptionFmt "inheritHeight" [])
                  ,DefaultFmt False          [])

  ,(OptionPairFmt (OptionFmt "hitbox"    [SomeArgFmt ArgFmtInt -- x
                                         ,SomeArgFmt ArgFmtInt -- y
                                         ,SomeArgFmt ArgFmtInt -- width
                                         ,SomeArgFmt ArgFmtInt -- height
                                         ])
                  (OptionFmt "emptyHitBox"  [])
                  ,DefaultFmt False      [])
  ]


type Stages = Map.Map Text Stage

-- given a path to a directory of stage directories, load all the stages
parseStages :: FilePath -> Renderer -> IO Stages
parseStages stagesPath renderer = do
  files <- listDirectory stagesPath

  -- Get the directories with a ".stage" extension
  let stageDirectories = filter ((== "stage") . extension) files

  -- Associate the NAME of all stage directories to their parsed Stage
  --mStages :: [(Text,Maybe (Stage Text))]
  mStages <- mapM (\stageDir -> (pack . name $ stageDir,) <$> parseStage stageDir stagesPath renderer) stageDirectories

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
parseStage :: FilePath -> FilePath -> Renderer -> IO (Maybe Stage)
parseStage stageDir stagesPath renderer = do
  res <- parseConfigFile stageConfigFmt (stagesPath ++ "/" ++ stageDir ++ "/stage")
  case res of
    -- Failed to parse stage file
    Left _
      -> return Nothing

    Right stageConfig
      -> do let gravity           = fromArgs "gravity"           (\[SomeArg (ArgInt x),SomeArg (ArgInt y)] -> Force $ V2 (conv x) (conv y)) (Force $ V2 0 0) stageConfig
                unitSize          = fromArgs "unitSize"          (\[SomeArg (ArgInt i)] -> conv i)                                          0                stageConfig
                baseThingsName    = fromArgs "baseThings"        (\[SomeArg (ArgText n)] -> n)                                              ""               stageConfig
                tileSetName       = fromArgs "tileset"           (\[SomeArg (ArgText n)] -> n)                                              ""               stageConfig
                subjectSpeedLimit = fromArgs "subjectSpeedLimit" (\[SomeArg (ArgInt x),SomeArg (ArgInt y)] -> V2 (conv x) (conv y))         (V2 100 100)     stageConfig
                thingSpeedLimit   = fromArgs "thingSpeedLimit"   (\[SomeArg (ArgInt x),SomeArg (ArgInt y)] -> V2 (conv x) (conv y))         (V2 100 100)     stageConfig
                subjectFriction   = fromArgs "subjectFriction"   (\[SomeArg (ArgInt i)] -> conv i)                                          1                stageConfig
                thingFriction     = fromArgs "thingFriction"     (\[SomeArg (ArgInt i)] -> conv i)                                          1                stageConfig

            -- Load the stages tileset
            tileset <- parseTileSet ("R/Tilesets/" ++ unpack tileSetName) renderer

            -- Get any tile aliases defined for this stage
            aliases    <- parseAliases tileset (stagesPath ++ "/" ++ stageDir)

            -- parse all the base things we might need to 'inherit' from
            baseThings <- parseThings ("R/Things/" ++ unpack baseThingsName) tileset unitSize

            -- parse all the things, and extract one named "player" to use as the subject
            things     <- parseThingInstances baseThings (stagesPath ++ "/" ++ stageDir)

            let mPlayer     = Map.lookup "player" things
                otherThings = Map.delete "player" things

            case mPlayer of
              -- No "player" tile
              Nothing
                -> return Nothing

              Just player
                -> do mBackground <- parseBackground (stagesPath ++ "/" ++ stageDir) tileset aliases unitSize renderer
                      case mBackground of
                        Nothing         -> return Nothing
                        Just background -> return $ setStage background player ((`zip` repeat exAgent) . Map.elems $ otherThings) gravity subjectSpeedLimit thingSpeedLimit subjectFriction thingFriction
  where
    conv :: Int -> CInt
    conv = toEnum . fromEnum

parseBackground :: FilePath -> TileSet -> Aliases -> CInt -> Renderer -> IO (Maybe Background )
parseBackground stagePath tileset aliases unitSize renderer = do
  mTexture <- loadTexture renderer (stagePath ++ "/background.bmp")
  mTileGrid <- parseTileGrid stagePath tileset aliases unitSize
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
parseThingInstances :: Things -> FilePath -> IO Things
parseThingInstances baseThings stagePath = do
  files <- listDirectory stagePath

  -- Get the files with a ".thinginstance" extension
  let thingInstanceFiles = filter ((== "thinginstance") . extension) files

  -- Associate the NAME of all thing instnace files to their parsed things
  -- mThingInstances :: [(Text,Maybe Thing)]
  mThingInstances <- mapM (\thingFile -> (pack . name $ thingFile,) <$> parseThingInstance baseThings thingFile stagePath) thingInstanceFiles

  -- Silently drop all where the config file failed to parse
  let thingInstances :: [(Text,Thing)]
      thingInstances = foldr (\(name,mThingInstance) acc -> case mThingInstance of
                                                              Nothing            -> acc
                                                              Just thingInstance -> (name,thingInstance):acc
                             )
                             []
                             mThingInstances

  return $ Map.fromList thingInstances

-- Given a context of base Things, a thing instance file under a stage filepath,
-- parse the thing instance file and create a thing, inheriting from the base thing.
parseThingInstance :: Things -> FilePath -> FilePath -> IO (Maybe Thing)
parseThingInstance baseThings thingInstanceFile stagePath = do
  res <- parseConfigFile thingInstanceConfigFmt (stagePath ++ "/" ++ thingInstanceFile)
  case res of
    -- Failed to parse thing instance file
    Left _
      -> return Nothing

    -- Parsed to a successful config with the given format
    Right thingInstanceConfig
      -> do -- Find the base thing it inherits from
           let mThingName = fromArgs "thing" (\[SomeArg (ArgText thingName)] -> Just thingName) Nothing thingInstanceConfig
           case mThingName of
             -- Default to failing if no inherited thing was specified
             Nothing -> return Nothing

             Just thingName
               -> let positionOffset = fromArgs "positioned" (\[SomeArg (ArgInt oX),SomeArg (ArgInt oY)]
                                                               -> V2 (conv oX) (conv oY)
                                                             ) (V2 0 0) thingInstanceConfig
                      velocity       = fromArgs "moving"     (\[SomeArg (ArgInt vX),SomeArg (ArgInt vY)]
                                                               -> V2 (conv vX) (conv vY)
                                                             ) (V2 0 0) thingInstanceConfig
                      hitBox         = fromArgs "hitbox"     (\[SomeArg (ArgInt x),SomeArg (ArgInt y),SomeArg (ArgInt w),SomeArg (ArgInt h)]
                                                               -> HitBoxRect (Rectangle (P $ V2 (conv x) (conv y)) (V2 (conv w) (conv h)))
                                                             ) NoHitBox thingInstanceConfig
                      mBaseThing = Map.lookup thingName baseThings
                     in -- If the base thing exists, inherit from it and modify by any config values
                       case mBaseThing of
                         -- The base thing doesnt exist. Fail!
                         Nothing
                           -> return Nothing

                         Just baseThing
                           -> do let thingWidth  = fromArgs "width"  (\[SomeArg (ArgInt w)] -> conv w) (baseThing^.thingTile.tileWidth)  thingInstanceConfig
                                     thingHeight = fromArgs "height" (\[SomeArg (ArgInt h)] -> conv h) (baseThing^.thingTile.tileHeight) thingInstanceConfig

                                 return . Just . set thingVelocity (Velocity velocity)
                                               . set (thingTile.tileWidth) thingWidth
                                               . set (thingTile.tileHeight) thingHeight
                                               . set (thingHitBox) hitBox
                                               . moveThingBy positionOffset
                                               $ baseThing

  where
    conv :: Int -> CInt
    conv = toEnum . fromEnum

