-- A camera has an image width and height and its own position
-- from which it can 'shoot'/ render to the screen an image of everything
-- that falls within the frame.
--
-- The camera can pan around and will not move past given absolute boundaries.
module Game.Camera
  (Camera()
  ,panRightBy,panLeftBy,panDownBy,panUpBy
  ,panRight,panLeft,panUp,panDown
  {-,panLeftEdge,panRightEdge,panTopEdge,panBottomEdge-}
  {-,panLeftBoundary,panRightBoundary,panTopBoundary,panBottomBoundary-}
  ,panTo
  ,mkCamera
  ,frameWidth,frameHeight

  ,Subject(..)
  ,shoot

  ,_trackSubject
  ) where

import Foreign.C.Types
import Linear hiding (trace)
import Linear.Affine
import SDL

import Game.Tile
import Game.Thing
import Game.Tiles
import Game.Stage

import Debug.Trace

data Camera = Camera
  {_panX :: CInt
  ,_panY :: CInt

  ,_width  :: CInt
  ,_height :: CInt

  -- Hard boundaries the camera will not move past
  ,_boundaryLeft  :: CInt
  ,_boundaryRight :: CInt
  ,_boundaryUp    :: CInt
  ,_boundaryDown  :: CInt

  ,_trackSubject   :: Bool
  }
  deriving Show

-- Convert a position relative to the world top-left to a position relative to the cameras
-- top-left
worldToCamera :: V2 CInt -> Camera -> V2 CInt
worldToCamera (V2 x y) c = V2 (x - (_panX c)) (y - (_panY c))

cameraToWorld :: V2 CInt -> Camera -> V2 CInt
cameraToWorld (V2 x y) c = V2 (x + (_panX c)) (y + (_panY c))


-- The width of the frame the camera is looking at
frameWidth :: Camera -> CInt
frameWidth = _width

-- The height of the frame the camera is looking at
frameHeight :: Camera -> CInt
frameHeight = _height

-- pan an amount in a direction 
-- ignores boundaries
panRightBy,panLeftBy,panDownBy,panUpBy :: CInt -> Camera -> Camera
panRightBy d c = let x' = (_panX c) + d in c{_panX = x'}
panLeftBy  d c = let x' = (_panX c) - d in c{_panX = x'}
panDownBy  d c = let y' = (_panY c) + d in c{_panY = y'}
panUpBy    d c = let y' = (_panY c) - d in c{_panY = y'}

-- pan a single unit in a direction 
-- ignores boundaries
panRight,panLeft,panUp,panDown :: Camera -> Camera
panRight = panRightBy 1
panLeft  = panLeftBy  1 
panDown  = panDownBy  1
panUp    = panUpBy    1

-- pan to an exact point in the world
panTo :: V2 CInt -> Camera -> Camera
panTo (V2 x y) c = c{_panX = x
                    ,_panY = y
                    }

-- pan to the bottom edge of the background tiles
{-panBottomEdge :: Camera -> Camera-}
{-panBottomEdge c =-}
  {-let tsHeight = tilesHeight (_tileRows . _background $ c)-}
      {-tileSize = _tileUnitSize . _background $ c-}
      {-bottomDistance = (tsHeight * tileSize) - (frameHeight c)-}
     {-in panDownBy (bottomDistance) c-}

-- pan to the left edge of the background tiles
{-panLeftEdge :: Camera -> Camera-}
{-panLeftEdge = undefined-}

{--- pan to the right edge of the background tiles-}
{-panRightEdge :: Camera -> Camera-}
{-panRightEdge = undefined-}

{--- pan to the top edge of the background tiles-}
{-panTopEdge :: Camera -> Camera-}
{-panTopEdge = undefined-}


-- pan to the left absolute boundary
{-panLeftBoundary :: Camera -> Camera-}
{-panLeftBoundary = undefined-}

{--- pan to the right absolute boundary-}
{-panRightBoundary :: Camera -> Camera-}
{-panRightBoundary = undefined-}

{--- pan to the top absolute boundary-}
{-panTopBoundary :: Camera -> Camera-}
{-panTopBoundary = undefined-}

{--- pan to the bottom absolute boundary-}
{-panBottomBoundary :: Camera -> Camera-}
{-panBottomBoundary = undefined-}

-- Pan as close as is allowed to the given point
-- (I.E. will end up at boundaries if they are exceeded)
panTowards :: V2 CInt -> Camera -> Camera
panTowards p c = panTo (closestPan p c) c

-- Return the position closest to the desired pan point
-- (I.E. x and y will either be as requested or the nearest border they are
-- past)
closestPan :: V2 CInt -> Camera -> V2 CInt
closestPan (V2 x y) c = V2 (closestPanX x c) (closestPanY y c)

-- Return the X point closest to the desired pan point
-- (I.E. either the left or right boundary or the given point)
closestPanX :: CInt -> Camera -> CInt 
closestPanX x c =
  if x < (_boundaryLeft c)
    then _boundaryLeft c 
    else if (_boundaryRight c) < x
           then _boundaryRight c
           else x

-- Return the Y point closest to the desired pan point
-- (I.E. either the top or bottom boundary or the given point)
closestPanY :: CInt -> Camera -> CInt
closestPanY y c =
  if y < (_boundaryUp c)
    then _boundaryUp c
    else if (_boundaryDown c) < y
           then _boundaryDown c
           else y



-- create a camera with:
-- - frame dimensions
-- - absolute boundaries
mkCamera :: V2 CInt -> V4 CInt -> Maybe Camera
mkCamera (V2 width height) (V4 bL bR bU bD) = Just $ Camera 0 0 width height bL bR bU bD False

-- shoot a frame of the scene, the background, the subject, any actors and props adjusted
-- for the cameras pan
shoot :: Ord t => Camera -> Renderer -> Stage t -> IO ()
shoot c renderer stage = do
  clear renderer

  -- Get the subject tile and attempt to pan to it
  let subjectTile = stageSubjectTile stage
  let subjectX = posX subjectTile
      subjectY = posY subjectTile
      c' = if _trackSubject c then panTowards (V2 subjectX subjectY) c else c

  -- render the background that falls within the frame
  renderTiles (V2 (_panX c') (_panY c'))
              (_width c',_height c')
              renderer
              (stageBackgroundTiles stage)

  -- render the subject within the frame
  renderTile renderer
             (mapPos (`worldToCamera` c') subjectTile)

  -- render the 'Thing's
  mapM_ (renderTile renderer . mapPos (`worldToCamera` c') . thingTile) (things stage)

  present renderer

