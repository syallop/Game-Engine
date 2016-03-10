module Game.Background
  (Background()
  ,mkBackground
  ,backgroundTiles
  ,backgroundTilesWide
  ,backgroundTilesTall
  ,backgroundTileRadius
  ,backgroundWidth
  ,backgroundHeight
  ) where

import Foreign.C.Types

import Game.Tiles

-- A Background is (currently) just 'Tiles'
data Background t = Background
  {_backgroundTiles :: Tiles t
  }
  deriving (Eq,Show)

mkBackground :: Tiles t -> Maybe (Background t)
mkBackground = Just . Background

-- The 'Tiles t' which make up a background
backgroundTiles :: Background t -> Tiles t
backgroundTiles = _backgroundTiles

-- The number of tiles wide the background is
backgroundTilesWide:: Background t -> CInt
backgroundTilesWide = tilesWidth . _tileRows . _backgroundTiles

-- The number of tiles tall the background is
backgroundTilesTall :: Background t -> CInt
backgroundTilesTall = tilesHeight . _tileRows . _backgroundTiles

-- The radius of each tile in the background
backgroundTileRadius :: Background t -> CInt
backgroundTileRadius = _tileUnitSize . _backgroundTiles

-- The absolute number of pixels wide the background is
backgroundWidth :: Background t -> CInt
backgroundWidth b = let ts = _backgroundTiles b
                       in ((tilesWidth . _tileRows $ ts) * _tileUnitSize ts)

-- The absolute number of pixels tall the background is
backgroundHeight :: Background t -> CInt
backgroundHeight b = let ts = _backgroundTiles b
                        in ((tilesHeight . _tileRows $ ts) * _tileUnitSize ts)

