{-# LANGUAGE TemplateHaskell #-}
module GameEngine.TileGrid
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

import GameEngine.HitBox
import GameEngine.Position
import GameEngine.Size
import GameEngine.Tile
import GameEngine.TileSet

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

renderTileGrid :: Pos -> Size -> Renderer -> TileGrid -> IO ()
renderTileGrid topLeft (Size (V2 frameWidth frameHeight)) renderer tileGrid = do
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
                                     -> do let pos = (Pos $ V2 (fromIntegral xPos) (fromIntegral yPos)) - topLeft
                                           -- TODO: Dont render things outside of the camera
                                           renderTile renderer $ mkTile tTy (Rectangle (let Pos p = pos in P p) (V2 (fromIntegral tileUnitSize) (fromIntegral tileUnitSize)))
                                           return (xPos+tileUnitSize)
                         )
                         (0 :: CInt)
                         (r^.row)
                 return (yPos+tileUnitSize)
         )
         (0 :: CInt) 
         (tileGrid^.tileGridRows.rows)


-- Does a Hitbox collide with the TileGrid?
collidesTileGrid :: HitBox -> TileGrid -> Bool
collidesTileGrid hb tg = any (\ix -> maybe False (view tileTypeIsSolid) $ indexTileType ix tg) $ coversIndexes hb tg

-- List of tile indexes covered by a HitBox in a TileGrid
coversIndexes :: HitBox -> TileGrid -> [V2 CInt]
coversIndexes hb tg = case coversIndexRange hb tg of

  -- HitBox isnt fully on the grid => Assume no collision
  Nothing
    -> []

  Just (V4 lX rX tY bY)
    -> [V2 x y | x <- [lX..rX], y <- [tY..bY]]

-- Which indexes in the TileGrid does a Tile cover
-- Nothing if falls outside of the range
-- The components 1,2 and 3,4 will always increase
coversIndexRange :: HitBox -> TileGrid -> Maybe (V4 CInt)
coversIndexRange hb tg = do
  V4 xLeft xRight yTop yBottom <- hitBoxBoundaries hb
  V2 xLeftIx  yTopIx    <- posToIndex (Pos $ V2 xLeft  yTop)    tg
  V2 xRightIx yBottomIx <- posToIndex (Pos $ V2 xRight yBottom) tg
  return $ V4 xLeftIx xRightIx yTopIx yBottomIx



-- Convert an absolute position to an index in the TileGrid
posToIndex :: Pos -> TileGrid -> Maybe (V2 CInt)
posToIndex p tg
  | indexInRange ixs tg = Just ixs
  | otherwise           = Nothing
  where
    ixs      = integralPosition $ p / (Pos $ V2 unitSize unitSize) 
    unitSize = fromIntegral $ tg^.tileGridUnitSize
    

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

