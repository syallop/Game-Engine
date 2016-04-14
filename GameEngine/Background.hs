{-# LANGUAGE TemplateHaskell #-}
module GameEngine.Background
  (Background()
  ,mkBackground
  ,backgroundTileGrid
  ,backgroundImage
  ) where

import Control.Lens
import Foreign.C.Types
import SDL

import GameEngine.TileGrid

-- A Background is (currently) just 'Tiles'
data Background = Background
  {_backgroundTileGrid :: TileGrid
  ,_backgroundImage    :: Maybe Texture
  }
  deriving (Eq)

makeLenses ''Background

instance Show Background where
  show (Background ts mi) = "Background " ++ show ts ++ maybe "NoBackgroundImage" (const "BackgroundImage") mi

mkBackground :: TileGrid -> Maybe Texture -> Maybe Background
mkBackground tileGrid mTexture = Just $ Background tileGrid mTexture

