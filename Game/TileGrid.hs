{-# LANGUAGE TemplateHaskell #-}
module Game.TileGrid
  (TileGrid()
  ,mkTileGrid
  ,tileGridRows
  ,tileGridTileSet
  ,tileGridUnitSize

  ,tileGridRowCount
  ,tileGridColumnCount
  ,tileGridHeight
  ,tileGridWidth

  ,renderTileGrid

  ,collidesTileGrid

  ,Rows(..),Row(..)
  ,row,rows
  )
  where

import Game.Tile
import Game.TileSet

import Control.Lens
import Control.Monad
import Data.Text (Text)
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL
import qualified Data.Map as M

newtype Row  = Row  {_row  :: [Text]} deriving (Eq,Show)
newtype Rows = Rows {_rows :: [Row] } deriving (Eq,Show)
makeLenses ''Row
makeLenses ''Rows


-- Tiles layed out in a grid have an associated TileSet and a
-- unitsize each tile is drawn at.
data TileGrid = TileGrid
  {_tileGridRows     :: Rows
  ,_tileGridTileSet  :: TileSet
  ,_tileGridUnitSize :: CInt
  }
  deriving (Eq,Show)
makeLenses ''TileGrid

-- Create a TileGrid on a TileSet where each tile has the same unitsize
--
-- TODO:
-- - Check all tiles mentioned in Rows have a mapping in the TileSet
-- > Check all rows have the same length?
mkTileGrid :: Rows -> TileSet -> CInt -> Maybe TileGrid
mkTileGrid rows tileSet size = Just TileGrid
  {_tileGridRows     = rows
  ,_tileGridTileSet  = tileSet
  ,_tileGridUnitSize = size
  }

-- The number of rows
tileGridRowCount :: TileGrid -> CInt
tileGridRowCount = toEnum . length . view (tileGridRows.rows)

-- The number of columns in the longest row
tileGridColumnCount :: TileGrid -> CInt
tileGridColumnCount = toEnum . maximum . map (length . _row) . view (tileGridRows.rows)

-- The absolute height of the tiles
tileGridHeight :: TileGrid -> CInt
tileGridHeight tg = tg^.tileGridUnitSize * tileGridRowCount tg

-- The absolute width of the longest row of tiles
tileGridWidth :: TileGrid -> CInt
tileGridWidth tg = tg^.tileGridUnitSize * tileGridColumnCount tg

renderTileGrid :: Point V2 CInt -> V2 CInt -> Renderer -> TileGrid -> IO ()
renderTileGrid topLeft (V2 frameWidth frameHeight) renderer tileGrid = do
  let tileUnitSize = tileGrid^.tileGridUnitSize
  -- TODO:
  -- - Due to the structure of Rows, and Row, as soon as we reach a tile we dont need to
  --   render we can stop checking the rest.
  -- - We could also calculate where to skip to begin the drawing instead of checking every tile
  --
  -- The datastructure representing tiles may be completly changed, perhaps to a zipper of zippers.
  -- Maybe change this first before adding the fiddly TODOs above.
  foldM_ (\yPos r
           -> do foldM_ (\xPos tileName
                          -> case lookupTileType tileName (tileGrid^.tileGridTileSet) of
                                 Nothing
                                     -> error "tile type has no info in tileset"

                                 Just tTy
                                     -> do let pos = (P $ V2 xPos yPos) - topLeft
                                           -- TODO: Dont render things outside of the camera
                                           renderTile renderer $ mkTile tTy (Rectangle pos (V2 tileUnitSize tileUnitSize))
                                           return (xPos+tileUnitSize)
                         )
                         (0 :: CInt)
                         (r^.row)
                 return (yPos+tileUnitSize)
         )
         (0 :: CInt)
         (tileGrid^.tileGridRows.rows)


-- Does a Tile collide with the TileGrid?
collidesTileGrid :: Tile -> TileGrid -> Bool
collidesTileGrid t tg = any (\ix -> maybe False (view tileTypeIsSolid) $ indexTileType ix tg) $ coversIndexes t tg

-- List of tile indexes covered by a Tile in a TileGrid
coversIndexes :: Tile -> TileGrid -> [V2 CInt]
coversIndexes t tg = case coversIndexRange t tg of

  -- Tile isnt fully on the grid => Assume no collision
  Nothing
    -> []

  Just (V4 lX rX tY bY)
    -> [V2 x y | x <- [lX..rX], y <- [tY..bY]]

-- Which indexes in the TileGrid does a Tile cover
-- Nothing if falls outside of the range
-- The components 1,2 and 3,4 will always increase
coversIndexRange :: Tile -> TileGrid -> Maybe (V4 CInt)
coversIndexRange t tg = do
  let tl = t^.tileTL
      br = t^.tileBR
  (V2 xLIx yTIx) <- posToIndex tl tg
  (V2 xRIx yBIx) <- posToIndex br tg
  return $ V4 xLIx xRIx yTIx yBIx

-- Convert an absolute position to an index in the TileGrid
posToIndex :: Point V2 CInt -> TileGrid -> Maybe (V2 CInt)
posToIndex (P (V2 x y)) tg
  | indexInRange (V2 xIx yIx) tg = Just $ V2 xIx yIx
  | otherwise                    = Nothing
  where
    xIx      = floor $ fromIntegral x / fromIntegral unitSize
    yIx      = floor $ fromIntegral y / fromIntegral unitSize
    unitSize = tg^.tileGridUnitSize

-- Is an index withing the TileGrid?
indexInRange :: V2 CInt -> TileGrid -> Bool
indexInRange (V2 xIx yIx) tg = (0 <= xIx) && (xIx <= tileGridColumnCount tg)
                            && (0 <= yIx) && (yIx <= tileGridRowCount tg)

-- Index the TileType of a position within the TileGrid
indexTileType :: V2 CInt -> TileGrid -> Maybe TileType
indexTileType (V2 xIx yIx) tg = do
  tileRow  <- indexTileRows (tg^.tileGridRows) yIx
  tileName <- indexTileRow  tileRow            xIx
  lookupTileType tileName (tg^.tileGridTileSet)
  where
    indexTileRows :: Rows -> CInt -> Maybe Row
    indexTileRows (Rows rs) = safeIndex rs

    indexTileRow :: Row -> CInt -> Maybe Text
    indexTileRow (Row r) = safeIndex r

    safeIndex :: [a] -> CInt -> Maybe a
    safeIndex []     _  = Nothing
    safeIndex (t:ts) ix
      | ix < 0    = Nothing
      | ix == 0   = Just t
      | otherwise = safeIndex ts (ix-1)

