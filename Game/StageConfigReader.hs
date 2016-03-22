{-# LANGUAGE GADTs, OverloadedStrings, TupleSections #-}
module Game.StageConfigReader where

import Game.ConfigReader
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt

import Game.Tile
import Game.Tiles
import Game.Thing
import Game.Stage
import Game.Velocity
import Game.Force
import Game.Background

import Game.ThingConfigReader
import Game.TileConfigReader

import SDL
import Foreign.C.Types
import Linear hiding (trace)

import Data.Maybe
import Data.Monoid
import Data.Text hiding (filter,foldr,map)
import Control.Applicative
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Text

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
  ]

aliasConfigFmt :: ConfigFmt
aliasConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "aliasFor" [SomeArgFmt ArgFmtText])
                  (OptionFmt "reserved" [])
                  ,DefaultFmt False [])
  ]

type Stages = Map.Map Text (Stage Text)

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
  let stages :: [(Text,Stage Text)]
      stages = foldr (\(name,mStage) acc -> case mStage of
                                              Nothing    -> acc
                                              Just stage -> (name,stage):acc
                     )
                     []
                     mStages

  return $ Map.fromList stages

-- given a path to a stage directory, load all its dependencies and assemble the stage.
parseStage :: FilePath -> FilePath -> Renderer -> IO (Maybe (Stage Text))
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
                        Just background -> return $ setStage background player (Map.elems otherThings) gravity
  where
    conv :: Int -> CInt
    conv = toEnum . fromEnum

parseBackground :: FilePath -> TileSet Text -> Aliases -> CInt -> Renderer -> IO (Maybe (Background Text))
parseBackground stagePath tileset aliases unitSize renderer = do
  mTexture <- loadTexture renderer (stagePath ++ "/background.bmp")
  mTiles <- parseTilesLayout stagePath tileset aliases unitSize
  case mTiles of
    -- failed to parse tiles, abort!
    -- TODO: could instead create empty tiles?
    --  - Only if layout file isnt present, not if its just invalid
    Nothing
      -> return Nothing

    Just tiles
      -> return . mkBackground tiles $ Just mTexture

type Aliases = Map.Map Text Alias
type Alias   = Maybe Text

-- Given a path to a directory of alias files and a tileset that may contain tileInfo that
-- is aliased, parse the Aliases
-- Note: Unspecified aliases are considered to be reserved words.
-- NAME.alias => the alias name
-- the contained "aliasFor" option specifies the TileInfo name aliased to
parseAliases :: TileSet Text -> FilePath -> IO Aliases
parseAliases tileset stagePath = do
  files <- listDirectory stagePath

  -- Get the files with a ".alias" extension
  let aliasFiles = filter ((== "alias") . extension) files

  -- Associate the NAME of all alias files to their parsed alias
  -- mAliases :: [(Text,Maybe (Maybe Text))]
  mAliases <- mapM (\aliasFile -> (pack . name $ aliasFile,) <$> parseAlias tileset aliasFile stagePath) aliasFiles

  -- Silently drop all where the config file failed to parse
  let aliases :: [(Text,Maybe Text)]
      aliases = foldr (\(name,mAlias) acc -> case mAlias of
                                               Nothing    -> acc
                                               Just alias -> (name,alias):acc
                      )
                      []
                      mAliases

  return $ Map.fromList aliases

-- Given a tileset to make aliases to, an alias file to parse under a stage filepath,
-- parse the alias, ensuring it exists in the tileset.
--
-- Nothing => Failure.
-- Just Nothing => Reserved
-- Just Just t => Correct alias
parseAlias :: TileSet Text -> FilePath -> FilePath -> IO (Maybe Alias)
parseAlias tileset aliasFile stagePath = do
  res <- parseConfigFile aliasConfigFmt (stagePath ++ "/" ++ aliasFile)
  case res of
    -- Failed to parse alias file
    Left _
      -> return Nothing

    Right aliasConfig
      -> if isSet "aliasFor" aliasConfig
           -- Alias is given, extract the aliased tilename
           then case getArgs "aliasFor" aliasConfig of
                  [SomeArg (ArgText tileName)]
                    -> if Map.member tileName tileset
                         -- Alias corresponds to tilename
                         then return $ Just $ Just tileName

                         -- Aliased tilename doesnt exist
                         else return Nothing
                  _ -> error "Didnt parse as claimed"

           -- Alias is reserved
           else return $ Just Nothing

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

                           mBaseThing = Map.lookup thingName baseThings
                         in -- If the base thing exists, inherit from it and modify by any config values
                            case mBaseThing of
                              -- The base thing doesnt exist. Fail!
                              Nothing
                                -> return Nothing

                              Just baseThing
                                -> return . Just . setVelocity (Velocity velocity) . moveThingBy positionOffset $ baseThing


                  _
                    -> error "Didnt parse as claimed"
           -- Default to failing if no inherited thing was specified
           else return Nothing
  where
    conv :: Int -> CInt
    conv = toEnum . fromEnum

-- Given a path to the stage directory containing a layout file,
-- parse the layout into a 'Tiles' where the format of the file is newline separated
-- rows of space separated names. Where names are first looked up in the tileset,
-- and if not found are looked up in the given aliases. The resulting name is the base name, not any alias that was used.
-- PARTIAL: All (used) aliases must be in the tileset names
parseTilesLayout :: FilePath -> TileSet Text -> Aliases -> CInt -> IO (Maybe (Tiles Text))
parseTilesLayout stagePath tileset aliases unitSize = do
  let names   = Map.keys tileset
      aliasMap :: Map.Map Text Text
      aliasMap = Map.fromList $ Map.foldrWithKey (\alias mV acc -> case mV of
                                                                     -- name is reserved
                                                                     Nothing -> acc
                                                                     Just n  -> (alias,n):acc
                                                 )
                                                 []
                                                 aliases

  res <- parseFromFile (tilesP names aliasMap tileset unitSize) (stagePath ++ "/layout")
  case res of
    -- Failed to parse layout file
    Left _
      -> return Nothing

    Right mTiles
      -> return mTiles


tilesP :: [Text] -> Map.Map Text Text -> TileSet Text -> CInt -> Parser (Maybe (Tiles Text))
tilesP names aliasMap tileset unitSize = mkTiles <$> rowsP names aliasMap <*> pure tileset <*> pure unitSize

rowsP :: [Text] -> Map.Map Text Text -> Parser (Rows Text)
rowsP names aliasMap = Rows <$> many (rowP names aliasMap <* newline)

rowP :: [Text] -> Map.Map Text Text -> Parser (Row Text)
rowP names aliasMap = (\r rs -> Row (r:rs)) <$> nameP names aliasMap <*> many (tileSepP *> nameP names aliasMap)

-- parse one of a tile name. Otherwise, parse a alias name. Return the base, unaliased name
nameP :: [Text] -> Map.Map Text Text -> Parser Text
nameP ns aliasMap =  regularNameP ns
                 <|> aliasNameP aliasMap

regularNameP :: [Text] -> Parser Text
regularNameP = (pack <$>) . choice . map (string' . unpack)

aliasNameP :: Map.Map Text Text -> Parser Text
aliasNameP aliasMap = do
  a <- regularNameP (Map.keys aliasMap)
  return . fromJust . Map.lookup a $ aliasMap

tileSepP = skipSome (choice [string' " ",string' "\t"])

