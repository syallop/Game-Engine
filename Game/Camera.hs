module Game.Camera
  (Camera()
  ,panX,panY
  ,panRight,panLeft,panUp,panDown
  ,panLeftEdge,panRightEdge,panTopEdge,panBottomEdge
  ,panLeftBoundary,panRightBoundary,panTopBoundary,panBottomBoundary
  ,mkCamera
  ,width,height

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

  -- dynamic things which have the chance to do things between frames
  --,_actors :: Actors
  }

data Subject = Subject
  {_subjectTile :: Tile
  }

width = _width
height = _height

-- pan a distance in either axis
panX d c = let x' = (_panX c) - d in if (_boundaryRight c) < x' then c else c{_panX = x'}
panY d c = let y' = (_panY c) + d in if (_boundaryUp c)    < y' then c else c{_panY = y'}

-- pan in various directions
panRight = panX 1
panLeft  = panX (-1)
panUp    = panY 1
panDown  = panY (-1)

-- pan to the edges of the tiles
panBottomEdge :: Camera t -> Camera t
panBottomEdge c =
  let tsHeight = tilesHeight (_tileColumns . _background $ c)
      tileSize = _tileUnitSize . _background $ c
      bottomDistance = (tsHeight * tileSize) - (height c)
     in panY (-1 * bottomDistance) c

panLeftEdge = undefined
panRightEdge = undefined
panTopEdge = undefined

-- pan to the absolute boundaries
panLeftBoundary = undefined
panRightBoundary = undefined
panTopBoundary = undefined
panBottomBoundary = undefined

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
  drawTile renderer (_subjectTile . _subject $ c)

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

