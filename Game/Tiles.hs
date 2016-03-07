module Game.Tiles
  ( Tiles()
  , TileInfo(..)
  , TileSet
  , mkTiles
  , renderTiles

  ,TileColumn(..),TileRow(..)

  ,tilesHeight
  ,tilesWidth

  ,_tileColumns
  ,_tileUnitSize
  )
  where

import SDL
import Linear
import Linear.Affine
import Foreign.C.Types
import GHC.Word

import Control.Monad
import qualified Data.Map as M
import Game.Tile

-- Features of a tile type
data TileInfo = TileInfo
  {_tileInfoColor :: TileColor
  }

-- Map elements of a tile type 't' to their info
type TileSet t = M.Map t TileInfo

-- A column of rows of some tile type 't'
newtype TileColumn t = TileColumn {_tileColumn :: [TileRow t]}
newtype TileRow t = TileRow {_tileRow :: [t]}

-- A specific collection of tiles 't' from some TileSet
data Tiles t = Tiles
  {_tileColumns  :: TileColumn t
  ,_tileSet      :: TileSet t
  ,_tileUnitSize :: CInt
  }

tilesHeight :: TileColumn t -> CInt
tilesHeight = toEnum . length . _tileColumn

tilesWidth :: TileColumn t -> CInt
tilesWidth  = toEnum . maximum . map (length . _tileRow) . _tileColumn

mkTiles :: TileColumn t -> TileSet t -> CInt -> Maybe (Tiles t)
mkTiles tileColumns tileset size
  = Just $ Tiles{_tileColumns  = tileColumns
                ,_tileSet      = tileset
                ,_tileUnitSize = size
                }

renderTiles :: Ord t => V2 CInt -> (CInt,CInt) -> Renderer -> Tiles t -> IO ()
renderTiles offset (screenWidth,screenHeight) renderer tiles = do
    let tileUnitSize = _tileUnitSize tiles
    foldM_ (\y row
             -> do foldM_ (\x tiletype
                            -> case M.lookup tiletype (_tileSet tiles) of
                                   Nothing
                                       -> error "tile type has no info in tileset"

                                   Just inf
                                       -> do let V2 x' y' = offset + (V2 x y)
                                                 pos      = P $ V2 x' y'

                                             -- Dont render things outside of the camera
                                             {-if (0 - tileUnitSize) < x' && x < screenWidth && 0 <= y' && y < screenHeight-}
                                               {-then-}
                                             drawTile renderer (tile (_tileInfoColor inf)
                                                                            pos
                                                                            tileUnitSize
                                                                      )
                                               {-else return ()-}
                                             return (x+tileUnitSize)
                           )
                           (0 :: CInt)
                           (_tileRow row)
                   return (y+tileUnitSize)
           )
           (0 :: CInt)
           (_tileColumn . _tileColumns $ tiles)



