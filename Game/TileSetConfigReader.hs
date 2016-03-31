{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TupleSections
  #-}
module Game.TileSetConfigReader
  (parseTileSet
  )
  where

import Game.ConfigReader
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt

import Game.Tile
import Game.TileConfigReader
import Game.TileSet

import Control.Applicative
import Data.Text hiding (map,dropWhile,drop,takeWhile,foldr,filter)
import SDL
import System.Directory
import qualified Data.Map as Map

-- Given a path to a directory of tile config files and possible textures,
-- load a tileset, associated to the tiles Text names.
-- Where:
-- - config files have the format specified by 'tileConfigFmt'
-- - config files are named 'name.tile'
-- - texture files are named 'name.bmp'
-- - config files may have no associated texture but currently vice-versa results in no tile.
--   TODO: When we have a texture but no config maybe parse the empty file/string => use all the defaults.
parseTileSet :: FilePath -> Renderer -> IO TileSet
parseTileSet tilesetPath renderer = do
  files <- listDirectory tilesetPath

  -- Partition the files into texture files and config files by their extension
  let textures,configs :: [FilePath]
      (textures,configs) = foldr (\file (accT,accC) -> case extension file of
                                                           "bmp"  -> (file:accT,accC)
                                                           "tile" -> (accT,file:accC)
                                                           _      -> (accT,accC)
                                 )
                                 ([],[])
                                 files

  -- Associate all config files with their possible texture file
  let tileFiles :: [(FilePath,Maybe FilePath)]
      tileFiles = map (\configFile -> if (name configFile ++ ".bmp") `elem` textures
                                        then (configFile,Just $ name configFile ++ ".bmp")
                                        else (configFile,Nothing)
                      )
                      configs

  -- Associate the NAME of all config files to their parsed tiletype
  -- :: [(Text,Maybe TileType)]
  mTileTypes <- mapM (\(cnfFile,mTexFile) -> (,) <$> (pure . pack $ name cnfFile)
                                                 <*> parseTileType ((,renderer) <$> mTexFile) cnfFile tilesetPath
                     )
                     tileFiles

  -- Silently drop all where the config file failed to parse
  let tileTypes :: [(Text,TileType)]
      tileTypes = foldr (\(name,mTileType) acc -> case mTileType of
                                                    Nothing       -> acc
                                                    Just tileType -> (name,tileType):acc
                        )
                        []
                        mTileTypes

  return $ mkTileSet $ Map.fromList tileTypes

