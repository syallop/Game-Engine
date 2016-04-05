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
      -> do let gravity
                    = if isSet "gravity" stageConfig
                        -- gravity is set
                        then case getArgs "gravity" stageConfig of
                               [SomeArg (ArgInt x),SomeArg (ArgInt y)]
                                 -> Force $ V2 (conv x) (conv y)
                               _ -> error "Didnt parse as claimed"
                        -- no gravity => 0 0
                        else Force $ V2 0 0

            let unitSize
                    = if isSet "unitSize" stageConfig
                        then case getArgs "unitSize" stageConfig of
                               [SomeArg (ArgInt i)] -> conv i
                               _ -> error "Didnt parse as claimed"
                        -- emptyUnitSize
                        else 1

            let baseThingsName
                    = if isSet "baseThings" stageConfig
                        then case getArgs "baseThings" stageConfig of
                               [SomeArg (ArgText n)] -> n
                               _ -> error "Didnt parse as claimed"
                        -- empty base things => use the empty string as a sentinel (parser never returns it)
                        else ""

            let tilesetName
                    = if isSet "tileset" stageConfig
                        then case getArgs "tileset" stageConfig of
                               [SomeArg (ArgText n)] -> n
                               _ -> error "Didnt parse as claimed"
                        -- empty tileset => use the empty string as a sentinel (parser never returns it)
                        else ""

            let subjectSpeedLimit
                    = if isSet "subjectSpeedLimit" stageConfig
                        then case getArgs "subjectSpeedLimit" stageConfig of
                               [SomeArg (ArgInt x), SomeArg (ArgInt y)] -> V2 (conv x) (conv y)
                               _ -> error "Didnt parse as claimed"
                        -- empty subject speed limit => default to... something too high!
                        else V2 100 100

            let thingSpeedLimit
                    = if isSet "thingSpeedLimit" stageConfig
                        then case getArgs "thingSpeedLimit" stageConfig of
                               [SomeArg (ArgInt x), SomeArg (ArgInt y)] -> V2 (conv x) (conv y)
                               _ -> error "Didnt parse as claimed"
                        -- empty thing speed limit => default to... something too high!
                        else V2 100 100

            let subjectFriction
                   = if isSet "subjectFriction" stageConfig
                       then case getArgs "subjectFriction" stageConfig of
                              [SomeArg (ArgInt fr)] -> conv fr
                              _ -> error "Didnt parse as claimed"
                       -- empty subject friction => default to 1!
                       else 1

            let thingFriction
                   = if isSet "thingFriction" stageConfig
                       then case getArgs "thingFriction" stageConfig of
                              [SomeArg (ArgInt fr)] -> conv fr
                              _ -> error "Didnt parse as claimed"
                       -- empty thing friction => default to 1!
                       else 1

            -- Load the stages tileset
            tileset <- parseTileSet ("R/Tilesets/" ++ unpack tilesetName) renderer

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
      -> -- Find the base thing it inherits from
         if isSet "thing" thingInstanceConfig
           then case getArgs "thing" thingInstanceConfig of
                  [SomeArg (ArgText thingName)]
                    -> let -- The amount the position is offset from the things default
                           positionOffset = if isSet "positioned" thingInstanceConfig
                                              then case getArgs "positioned" thingInstanceConfig of
                                                     [SomeArg (ArgInt oX),SomeArg (ArgInt oY)]
                                                       -> V2 (conv oX) (conv oY)
                                                     _
                                                       -> error "Didnt parse as claimed"
                                              -- Unpositioned => no offset from thing default
                                              else V2 0 0
                           -- The exact initial velocity
                           velocity :: V2 CInt
                           velocity       = if isSet "moving" thingInstanceConfig
                                              then case getArgs "moving" thingInstanceConfig of
                                                     [SomeArg (ArgInt vX),SomeArg (ArgInt vY)]
                                                       -> V2 (conv vX) (conv vY)
                                                     _
                                                       -> error "Didnt parse as claimed"
                                              -- Stationary => no velocity
                                              else V2 0 0

                           hitBox :: HitBox
                           hitBox        = if isSet "hitbox" thingInstanceConfig
                                             then case getArgs "hitbox" thingInstanceConfig of
                                                    [SomeArg (ArgInt x),SomeArg (ArgInt y),SomeArg (ArgInt w),SomeArg (ArgInt h)]
                                                      -> HitBoxRect (Rectangle (P $ V2 (conv x) (conv y)) (V2 (conv w) (conv h)))

                                             -- no hitbox
                                             else NoHitBox

                           mBaseThing = Map.lookup thingName baseThings
                         in -- If the base thing exists, inherit from it and modify by any config values
                            case mBaseThing of
                              -- The base thing doesnt exist. Fail!
                              Nothing
                                -> return Nothing

                              Just baseThing
                                -> do let thingWidth :: CInt
                                          thingWidth = if isSet "width" thingInstanceConfig
                                                         then case getArgs "width" thingInstanceConfig of
                                                                [SomeArg (ArgInt w)] -> conv w
                                                                _                    -> error "Didnt parse as claimed"
                                                         -- InheritWidth
                                                         else baseThing^.thingTile.tileWidth

                                          thingHeight :: CInt
                                          thingHeight = if isSet "height" thingInstanceConfig
                                                          then case getArgs "height" thingInstanceConfig of
                                                                 [SomeArg (ArgInt h)] -> conv h
                                                                 _                    -> error "Didnt parse as claimed"
                                                          -- InheritHeight
                                                          else baseThing^.thingTile.tileHeight

                                      return . Just . set thingVelocity (Velocity velocity)
                                                    . set (thingTile.tileWidth) thingWidth
                                                    . set (thingTile.tileHeight) thingHeight
                                                    . set (thingHitBox) hitBox
                                                    . moveThingBy positionOffset
                                                    $ baseThing


                  _
                    -> error "Didnt parse as claimed"
           -- Default to failing if no inherited thing was specified
           else return Nothing
  where
    conv :: Int -> CInt
    conv = toEnum . fromEnum

