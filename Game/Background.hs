{-# LANGUAGE TemplateHaskell #-}
module Game.Background
  (Background()
  ,mkBackground
  ,backgroundTiles
  ,backgroundImage
  ,backgroundTilesWide
  ,backgroundTilesTall
  ,backgroundTileRadius
  ,backgroundWidth
  ,backgroundHeight
  ) where

import Control.Lens
import Foreign.C.Types
import SDL

import Game.Tiles

-- A Background is (currently) just 'Tiles'
data Background t = Background
  {_backgroundTiles :: Tiles t
  ,_backgroundImage :: Maybe Texture
  }
  deriving (Eq)

makeLenses ''Background

instance Show t => Show (Background t) where
  show (Background ts mi) = "Background " ++ show ts ++ maybe "NoBackgroundImage" (const "BackgroundImage") mi

mkBackground :: Tiles t -> Maybe Texture -> Maybe (Background t)
mkBackground tiles mTexture = Just $ Background tiles mTexture

-- The number of tiles wide the background is
backgroundTilesWide:: Background t -> CInt
backgroundTilesWide = tilesWidth . _tilesRows . _backgroundTiles

-- The number of tiles tall the background is
backgroundTilesTall :: Background t -> CInt
backgroundTilesTall = tilesHeight . _tilesRows . _backgroundTiles

-- The radius of each tile in the background
backgroundTileRadius :: Background t -> CInt
backgroundTileRadius = _tilesUnitSize . _backgroundTiles

-- The absolute number of pixels wide the background is
backgroundWidth :: Background t -> CInt
backgroundWidth b = let ts = _backgroundTiles b
                       in ((tilesWidth . _tilesRows $ ts) * _tilesUnitSize ts)

-- The absolute number of pixels tall the background is
backgroundHeight :: Background t -> CInt
backgroundHeight b = let ts = _backgroundTiles b
                        in ((tilesHeight . _tilesRows $ ts) * _tilesUnitSize ts)

