{-# LANGUAGE GADTs, OverloadedStrings, TupleSections #-}
module Game.TileConfigReader
  (tileConfigFmt
  ,parseTileSet
  ,parseTileInfo
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
import Game.Tiles

import SDL
import Linear
import Foreign.C.Types
import GHC.Word

import System.Directory
import Control.Applicative
import Data.Text hiding (map,dropWhile,drop,takeWhile,foldr,filter)
import Data.Maybe
import Data.Monoid
import qualified Data.Map as Map

-- Tiles config file specifies:
-- - whether they are solid or not
-- - an optional color to be used if no texture is found
tileConfigFmt :: ConfigFmt
tileConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "solid"      [])
                  (OptionFmt "intangible" [])
                  ,DefaultFmt False       [])

  ,(OptionPairFmt (OptionFmt "colored"   [SomeArgFmt ArgFmtInt
                                         ,SomeArgFmt ArgFmtInt
                                         ,SomeArgFmt ArgFmtInt
                                         ])
                  (OptionFmt "colorless" [])
                  ,DefaultFmt True       [SomeArg $ ArgInt 0
                                         ,SomeArg $ ArgInt 0
                                         ,SomeArg $ ArgInt 0
                                         ])
  ]

-- Given a path to a directory of tile config files and possible textures,
-- load a tileset, associated to the tiles Text names.
-- Where:
-- - config files have the format specified by 'tileConfigFmt'
-- - config files are named 'name.tile'
-- - texture files are named 'name.bmp'
-- - config files may have no associated texture but currently vice-versa results in no tile.
--   TODO: When we have a texture but no config maybe parse the empty file/string => use all the defaults.
parseTileSet :: FilePath -> Renderer -> IO (TileSet Text)
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
      tileFiles = map (\configFile -> if elem ((name configFile) ++ ".bmp") textures
                                        then (configFile,Just $ (name configFile) ++ ".bmp")
                                        else (configFile,Nothing)
                      )
                      configs

  -- Associate the NAME of all config files to their parsed tile info
  -- :: [(Text,Maybe TileInfo)]
  mTileInfos <- mapM (\(cnfFile,mTexFile) -> (,) <$> (pure . pack $ name cnfFile)
                                                 <*> parseTileInfo ((,renderer) <$> mTexFile) cnfFile tilesetPath
                     )
                     tileFiles

  -- Silently drop all where the config file failed to parse
  let tileInfos :: [(Text,TileInfo)]
      tileInfos = foldr (\(name,mTileInfo) acc -> case mTileInfo of
                                                    Nothing       -> acc
                                                    Just tileInfo -> (name,tileInfo):acc
                        )
                        []
                        mTileInfos

  return $ Map.fromList tileInfos

-- Given a possible path to a texture file, a path to a tile config file and a renderer,
-- attempt to parse the config file, load the texture if present and return the TileInfo.
parseTileInfo :: Maybe (FilePath,Renderer) -> FilePath -> FilePath -> IO (Maybe TileInfo)
parseTileInfo mTexture configFile tilesetPath = do
  res <- parseConfigFile tileConfigFmt (tilesetPath ++ "/" ++ configFile)
  case res of
    -- Failed to parse tile config file
    Left _
      -> return Nothing

    -- Successful parse, means we informally know each of the required options is set
    -- and has args in the correct format. Partial functions!
    Right tileConfig
      -> do let mIOTexture = (\(texFile,renderer) -> loadTexture renderer (tilesetPath ++ "/" ++ texFile)) <$> mTexture
            let isSolid    = isSet "solid" tileConfig
            case mIOTexture of
              Nothing
                -> if isSet "colored" tileConfig
                     then case getArgs "colored" tileConfig of
                            [SomeArg (ArgInt r), SomeArg (ArgInt g)
                                               , SomeArg (ArgInt b)
                                               , SomeArg (ArgInt a)
                                               ] -> return $ Just $ InfoColored (V4 (conv r) (conv g) (conv b) (conv a)) isSolid
                            _                    -> error "Didnt actually parse config correctly.."
                     else return $ Just $ InfoInvisible isSolid

              Just ioTexture
                -> do texture <- ioTexture
                      return $ Just $ InfoTextured texture isSolid
  where
    conv :: Int -> Word8
    conv = toEnum . fromEnum

