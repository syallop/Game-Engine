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
    }
  | InfoTextured
    {_tileInfoTexture :: Texture
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
  InfoColored c
    -> colorTile c pos radius

  InfoTextured t
    -> textureTile t pos radius

