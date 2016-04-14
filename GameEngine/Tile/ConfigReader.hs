{-# LANGUAGE
    GADTs
  , OverloadedStrings
  , TupleSections
  #-}
module GameEngine.Tile.ConfigReader
  (tileConfigFmt
  ,parseTileType
  )
  where

import GameEngine.ConfigReader
import GameEngine.ConfigReader.Config
import GameEngine.ConfigReader.ConfigFmt
import GameEngine.ConfigReader.Arg
import GameEngine.ConfigReader.ArgFmt
import GameEngine.ConfigReader.Option
import GameEngine.ConfigReader.OptionFmt

import GameEngine.Tile

import Foreign.C.Types
import GHC.Word
import Linear
import SDL

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Text hiding (map,dropWhile,drop,takeWhile,foldr,filter)
import System.Directory
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

-- Given a possible path to a texture file, a path to a tile config file and a renderer,
-- attempt to parse the config file, load the texture if present and return the TileInfo.
parseTileType :: Maybe (FilePath,Renderer) -> FilePath -> FilePath -> IO (Maybe TileType)
parseTileType mTexture configFile tilesetPath = do
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
                -> return $ Just $ fromArgs "colored" (\[SomeArg (ArgInt r),SomeArg (ArgInt g),SomeArg (ArgInt b), SomeArg (ArgInt a)] -> TileTypeColored (V4 (conv r) (conv g) (conv b) (conv a)) isSolid)
                                                      (TileTypeInvisible isSolid)
                                                      tileConfig

              Just ioTexture
                -> do texture <- ioTexture
                      return $ Just $ TileTypeTextured texture isSolid
  where
    conv :: Int -> Word8
    conv = toEnum . fromEnum

