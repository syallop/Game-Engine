{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TupleSections
  #-}
module GameEngine.TileGrid.ConfigReader
  (parseTileGrid
  ,Aliases
  ,Alias
  ,aliasConfigFmt
  ,parseAlias
  ,parseAliases
  )
  where

import GameEngine.ConfigReader
import GameEngine.ConfigReader.Arg
import GameEngine.ConfigReader.ArgFmt
import GameEngine.ConfigReader.Config
import GameEngine.ConfigReader.ConfigFmt
import GameEngine.ConfigReader.Option
import GameEngine.ConfigReader.OptionFmt

import GameEngine.TileGrid
import GameEngine.TileSet

import Control.Applicative
import Data.Maybe
import Data.Text hiding (filter,foldr,map,zip)
import Foreign.C.Types
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Data.Map as Map

type Aliases = Map.Map Text Alias
type Alias   = Maybe Text

aliasConfigFmt :: ConfigFmt
aliasConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "aliasFor" [SomeArgFmt ArgFmtText])
                  (OptionFmt "reserved" [])
                  ,DefaultFmt False [])
  ]

-- Given a path to a directory of alias files and a tileset that may contain tileInfo that
-- is aliased, parse the Aliases
-- Note: Unspecified aliases are considered to be reserved words.
-- NAME.alias => the alias name
-- the contained "aliasFor" option specifies the TileInfo name aliased to
parseAliases :: TileSet -> FilePath -> IO Aliases
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
parseAlias :: TileSet -> FilePath -> FilePath -> IO (Maybe Alias)
parseAlias tileSet aliasFile stagePath = do
  res <- parseConfigFile aliasConfigFmt (stagePath ++ "/" ++ aliasFile)
  return $ case res of
    -- Failed to parse alias file
    Left _
      -> Nothing

    Right aliasConfig
      -> fromArgs "aliasFor" (\[SomeArg (ArgText tileName)] -> if memberTileName tileName tileSet then Just $ Just tileName else Nothing) (Just Nothing) aliasConfig

-- Given a path to the stage directory containing a layout file,
-- parse the layout into a 'Tiles' where the format of the file is newline separated
-- rows of space separated names. Where names are first looked up in the tileset,
-- and if not found are looked up in the given aliases. The resulting name is the base name, not any alias that was used.
-- PARTIAL: All (used) aliases must be in the tileset names
parseTileGrid :: FilePath -> TileSet -> Aliases -> CInt -> IO (Maybe TileGrid)
parseTileGrid stagePath tileset aliases unitSize = do
  let names   = tileNames tileset
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

tilesP :: [Text] -> Map.Map Text Text -> TileSet -> CInt -> Parser (Maybe TileGrid)
tilesP names aliasMap tileset unitSize = mkTileGrid <$> rowsP names aliasMap <*> pure tileset <*> pure unitSize

rowsP :: [Text] -> Map.Map Text Text -> Parser Rows
rowsP names aliasMap = Rows <$> many (rowP names aliasMap <* newline)

rowP :: [Text] -> Map.Map Text Text -> Parser Row
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

