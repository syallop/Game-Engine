-- A camera has an image width and height and its own position
-- from which it can 'shoot'/ render to the screen an image of everything
-- that falls within the frame.
--
-- The camera can pan around and will not move past given absolute boundaries.
--
-- Currently the Camera maintains the background tiles and the subject... basically it is responsible
-- for holding everything in the larger scene and only rendering what it can see, offset by the correct amounts.
-- In the future, the 'Scene' might be abstracted. Maybe not.
module Game.Camera
  (Camera()
  ,panX,panY
  ,panRight,panLeft,panUp,panDown
  ,panLeftEdge,panRightEdge,panTopEdge,panBottomEdge
  ,panLeftBoundary,panRightBoundary,panTopBoundary,panBottomBoundary
  ,mkCamera
  ,frameWidth,frameHeight

  ,Subject(..)
  ,moveSubjectRight
  ,moveSubjectLeft

  ,shoot
  ) where

import Foreign.C.Types
import Linear
import Linear.Affine
import SDL

import Game.Tile
import Game.Tiles

data Camera t = Camera
  {_panX :: CInt
  ,_panY :: CInt

  ,_width  :: CInt
  ,_height :: CInt

  -- Hard boundaries the camera will not move past
  ,_boundaryLeft  :: CInt
  ,_boundaryRight :: CInt
  ,_boundaryUp    :: CInt
  ,_boundaryDown  :: CInt

  -- The tiles which make up the background
  ,_background :: Tiles t

  -- The 'player'. The camera tracks their movement if possible
  ,_subject :: Subject
  }

data Subject = Subject
  {_subjectTile :: Tile
  }

-- The width of the frame the camera is looking at
frameWidth :: Camera t -> CInt
frameWidth = _width

-- The height of the frame the camera is looking at
frameHeight :: Camera t -> CInt
frameHeight = _height

-- pan a distance right in the X axis
panX :: CInt -> Camera t -> Camera t
panX d c = let x' = (_panX c) - d in if (_boundaryRight c) < x' then c else c{_panX = x'}

-- pan a distance down in the Y axis
panY :: CInt -> Camera t -> Camera t
panY d c = let y' = (_panY c) + d in if (_boundaryUp c)    < y' then c else c{_panY = y'}


-- pan a single unit right
panRight :: Camera t -> Camera t
panRight = panX 1

-- pan a single unit left
panLeft :: Camera t -> Camera t
panLeft  = panX (-1)

-- pan a single unit up
panUp :: Camera t -> Camera t
panUp    = panY 1

-- pan a single unit down
panDown :: Camera t -> Camera t
panDown  = panY (-1)


-- pan to the bottom edge of the background tiles
panBottomEdge :: Camera t -> Camera t
panBottomEdge c =
  let tsHeight = tilesHeight (_tileColumns . _background $ c)
      tileSize = _tileUnitSize . _background $ c
      bottomDistance = (tsHeight * tileSize) - (frameHeight c)
     in panY (-1 * bottomDistance) c

-- pan to the left edge of the background tiles
panLeftEdge :: Camera t -> Camera t
panLeftEdge = undefined

-- pan to the right edge of the background tiles
panRightEdge :: Camera t -> Camera t
panRightEdge = undefined

-- pan to the top edge of the background tiles
panTopEdge :: Camera t -> Camera t
panTopEdge = undefined


-- pan to the left absolute boundary
panLeftBoundary :: Camera t -> Camera t
panLeftBoundary = undefined

-- pan to the right absolute boundary
panRightBoundary :: Camera t -> Camera t
panRightBoundary = undefined

-- pan to the top absolute boundary
panTopBoundary :: Camera t -> Camera t
panTopBoundary = undefined

-- pan to the bottom absolute boundary
panBottomBoundary :: Camera t -> Camera t
panBottomBoundary = undefined

-- create a camera with:
-- - frame dimensions
-- - absolute boundaries
-- - background tiles
-- - a main subject
--
-- TODO:
-- - Either all tiles should be within the boundaries, or the boundaries should be able to be changed,
--   or maybe temporarily locked and unlocked => The subject cannot leave an area until something happens despite
mkCamera :: V2 CInt -> V4 CInt -> Tiles t -> Subject -> Maybe (Camera t)
mkCamera (V2 width height) (V4 bL bR bU bD) tiles subject = Just $ Camera 0 0 width height bL bR bU bD tiles subject

-- establish the background tiles
background :: Tiles t -> Camera t -> Camera t
background = undefined

-- shoot a frame of the scene, the background, the subject, any actors and props adjusted
-- for the cameras pan
shoot :: Ord t => Camera t -> Renderer -> IO ()
shoot c renderer = do
  clear renderer

  -- render the background that falls within the frame
  renderTiles (V2 (_panX c) (_panY c)) (_width c,_height c) renderer (_background c)

  -- render the subject within the frame
  renderTile renderer (_subjectTile . _subject $ c)

  present renderer

-- TODO
-- move the subject right if:
-- - They do not leave the boundary
-- - They do not collide with something solid
-- - They do not collide with something harmful
-- Pan the camera right to follow them if possible.
moveSubjectRight :: Camera t -> Camera t
moveSubjectRight c = panRight c

moveSubjectLeft :: Camera t -> Camera t
moveSubjectLeft c = panLeft c

