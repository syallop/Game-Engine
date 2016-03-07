-- 'Tiles' are a specific collection of 'Tile's from some 'TileSet'.
-- A 'TileSet' maps some tile type 't' onto 'TileInfo' which describes general properties
-- that all tiles of that type have.
module Game.Tiles
  ( Tiles()
  ,_tileColumns
  ,_tileUnitSize

  , TileInfo(..)
  , tileInfoInstance

  , TileSet
  , mkTiles
  , renderTiles

  ,TileColumn(..),TileRow(..)
  ,tilesHeight
  ,tilesWidth
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
data TileInfo
  = InfoColored
    {_tileInfoColor   :: TileColor
    ,_tileSolid       :: Bool
    }
  | InfoTextured
    {_tileInfoTexture :: Texture
    ,_tileSolid       :: Bool
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

-- The height is the number of rows
tilesHeight :: TileColumn t -> CInt
tilesHeight = toEnum . length . _tileColumn

-- The width is the longest row
tilesWidth :: TileColumn t -> CInt
tilesWidth  = toEnum . maximum . map (length . _tileRow) . _tileColumn

-- Create Tiles on a TileSet, each with a defined unit size.
--
-- TODO:
-- - Check all tiles mentioned in a TileColumn have info in the TileSet.
-- ? Check all the rows in the TileColumn have the same length?
mkTiles :: TileColumn t -> TileSet t -> CInt -> Maybe (Tiles t)
mkTiles tileColumns tileset size
  = Just $ Tiles{_tileColumns  = tileColumns
                ,_tileSet      = tileset
                ,_tileUnitSize = size
                }

-- Render 'Tiles' ordered left->right, top-> bottom, offset by a given quantity and with knowledge
-- screen width being drawn into, such that tiles which fall outside the screen may be skipped
renderTiles :: Ord t => V2 CInt -> (CInt,CInt) -> Renderer -> Tiles t -> IO ()
renderTiles offset (screenWidth,screenHeight) renderer tiles = do
    let tileUnitSize = _tileUnitSize tiles
    -- TODO:
    -- - Due to the structure of TileColumns, and TileRows, as soon as we reach a tile we dont need to
    --   render we can stop checking the rest.
    -- - We could also calculate where to skip to begin the drawing instead of checking every tile
    --
    -- The datastructure representing tiles may be completly changed, perhaps to a zipper of zippers.
    -- Maybe change this first before adding the fiddly TODOs above.
    foldM_ (\y row
             -> do foldM_ (\x tiletype
                            -> case M.lookup tiletype (_tileSet tiles) of
                                   Nothing
                                       -> error "tile type has no info in tileset"

                                   Just inf
                                       -> do let V2 x' y' = offset + V2 x y
                                                 pos      = P $ V2 x' y'
                                             -- TODO: Dont render things outside of the camera
                                             renderTile renderer $ tileInfoInstance inf pos tileUnitSize
                                             return (x+tileUnitSize)
                           )
                           (0 :: CInt)
                           (_tileRow row)
                   return (y+tileUnitSize)
           )
           (0 :: CInt)
           (_tileColumn . _tileColumns $ tiles)

-- Make a new instance tile from a tile info at the given position and radius
tileInfoInstance :: TileInfo -> Point V2 CInt -> CInt -> Tile
tileInfoInstance inf pos radius = case inf of
  InfoColored c _
    -> colorTile c pos radius

  InfoTextured t _
    -> textureTile t pos radius


-- Does a tile collide with any solid tiles it would cover?
collides :: Ord t => Tile -> Tiles t -> Bool
collides t ts = case covers t ts of

  -- No tiles covered => out of range => no collision
  Nothing
    -> False

  -- A range of tiles are covered. If any of them are solid => collision
  Just range
    -> any (`isSolidAt` ts) (indexesCovered range)

-- The tile indexes covered by a range V4 x0 x1 y0 y1
indexesCovered :: V4 CInt -> [V2 CInt]
indexesCovered (V4 x0 x1 y0 y1) = [V2 x y | x <- [x0..x1], y <- [y0..y1]]

-- a range of x indexes, then y indexes a tile would cover if placed over tiles.
-- Nothing if it falls outside the range.
-- The components 1,2 and 3,4 will always increase
covers :: Tile -> Tiles t -> Maybe (V4 CInt)
covers t ts = do
  let tRadius = radius t
      x0 = posX t
      x1 = x0 + tRadius
      y0 = posY t
      y1 = y0 + tRadius
      unitSize = _tileUnitSize ts

      tl = V2 x0 y0
      br = V2 x1 y1

  (V2 x0Ix y0Ix) <- posToTileIx tl ts
  (V2 x1Ix y1Ix) <- posToTileIx br ts
  return $ V4 x0Ix x1Ix y0Ix y1Ix

-- convert a position into an index within a tile row/ column
posToTileIx :: V2 CInt -> Tiles t -> Maybe (V2 CInt)
posToTileIx (V2 x y) ts =
  let unitSize = _tileUnitSize ts
      xIx = floor $ (fromIntegral x) / (fromIntegral unitSize)
      yIx = floor $ (fromIntegral y) / (fromIntegral unitSize)
     in if indexInRange (V2 xIx yIx) ts
          then Just (V2 xIx yIx) -- within tile range
          else Nothing           -- outside tile range

-- Is a tileIndex within the tiles?
indexInRange :: V2 CInt -> Tiles t -> Bool
indexInRange (V2 xIx yIx) ts = (0 <= xIx) && (xIx < (tilesWidth . _tileColumns $ ts))
                            && (0 <= yIx) && (yIx < (tilesHeight . _tileColumns $ ts))

-- Is a tile solid at the given index? False if out of range.
isSolidAt :: Ord t => V2 CInt -> Tiles t -> Bool
isSolidAt ix ts = maybe False isSolid $ indexTileInfo ix ts

-- Is a tile solid?
isSolid :: TileInfo -> Bool
isSolid = _tileSolid

-- Index the tileinfo at row,column
indexTileInfo :: Ord t => V2 CInt -> Tiles t -> Maybe TileInfo
indexTileInfo (V2 rowIx colIx) ts = do
  tileRow <- indexTileColumn (_tileColumns ts) colIx
  t       <- indexTileRow tileRow rowIx
  M.lookup t (_tileSet ts)

-- index a row from a 'tilecolumn'
indexTileColumn :: TileColumn t -> CInt -> Maybe (TileRow t)
indexTileColumn (TileColumn [])     ix = Nothing
indexTileColumn (TileColumn (r:rs)) ix
  | ix < 0    = Nothing
  | ix == 0   = Just r
  | otherwise = indexTileColumn (TileColumn rs) ix

-- index a t from a row
indexTileRow :: TileRow t -> CInt -> Maybe t
indexTileRow (TileRow [])     _ = Nothing
indexTileRow (TileRow (t:ts)) ix
  | ix < 0    = Nothing
  | ix == 0   = Just t
  | otherwise = indexTileRow (TileRow ts) (ix-1)

