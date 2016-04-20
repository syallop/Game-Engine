{-# LANGUAGE TemplateHaskell #-}
-- A camera has an image width and height and its own position
-- from which it can 'shoot'/ render to the screen an image of everything
-- that falls within the frame.
--
-- The camera can pan around and will not move past given absolute boundaries.
module GameEngine.Camera
  (Camera()
  ,panRightBy,panLeftBy,panDownBy,panUpBy
  ,panRight,panLeft,panUp,panDown
  {-,panLeftEdge,panRightEdge,panTopEdge,panBottomEdge-}
  {-,panLeftBoundary,panRightBoundary,panTopBoundary,panBottomBoundary-}
  ,panTo
  ,mkCamera

  ,Subject(..)
  ,shoot

  ,_cameraTrackSubject

  ,setBoundaries

  ,cameraPanX
  ,cameraPanY
  ,cameraWidth
  ,cameraBoundaryLeft
  ,cameraBoundaryUp
  ,cameraBoundaryDown
  ,cameraTrackSubject
  ) where

import Control.Lens
import Foreign.C.Types
import Linear hiding (trace)
import Linear.Affine
import SDL

import GameEngine.AI
import GameEngine.Background
import GameEngine.Collect
import GameEngine.Position
import GameEngine.Size
import GameEngine.Stage
import GameEngine.Thing
import GameEngine.Tile
import GameEngine.TileGrid

import Debug.Trace

data Camera = Camera
  {_cameraPan            :: Pos 

  ,_cameraDimensions     :: Size 

  -- Hard boundaries the camera will not move past
  ,_cameraBoundaries     :: V4 CFloat

  ,_cameraTrackSubject   :: Bool
  }
  deriving Show

makeLenses ''Camera

cameraPanX :: Lens' Camera CFloat
cameraPanX = cameraPan.pos._x

cameraPanY :: Lens' Camera CFloat
cameraPanY = cameraPan.pos._y

cameraWidth :: Lens' Camera CFloat
cameraWidth = cameraDimensions.size._x

cameraHeight :: Lens' Camera CFloat
cameraHeight = cameraDimensions.size._y

cameraBoundaryLeft :: Lens' Camera CFloat
cameraBoundaryLeft = cameraBoundaries._x

cameraBoundaryRight :: Lens' Camera CFloat
cameraBoundaryRight = cameraBoundaries._y

cameraBoundaryUp :: Lens' Camera CFloat
cameraBoundaryUp = cameraBoundaries._z

cameraBoundaryDown :: Lens' Camera CFloat
cameraBoundaryDown = cameraBoundaries._w

-- Convert a position relative to the world top-left to a position relative to the cameras
-- top-left
worldToCamera :: Pos -> Camera -> Pos 
worldToCamera (Pos (V2 x y)) c = Pos $ V2 (x - c^.cameraPanX) (y - c^.cameraPanY)

cameraToWorld :: Pos -> Camera -> Pos 
cameraToWorld (Pos (V2 x y)) c = Pos $ V2 (x + (c^.cameraPanX)) (y + (c^.cameraPanY))


-- pan an amount in a direction
-- ignores boundaries
panRightBy,panLeftBy,panDownBy,panUpBy :: CFloat -> Camera -> Camera
panRightBy = over cameraPanX . (+)
panLeftBy  = over cameraPanX . flip (-)
panDownBy  = over cameraPanY . (+)
panUpBy    = over cameraPanY . flip (-)

-- pan a single unit in a direction
-- ignores boundaries
panRight,panLeft,panUp,panDown :: Camera -> Camera
panRight = panRightBy 1
panLeft  = panLeftBy  1
panDown  = panDownBy  1
panUp    = panUpBy    1

-- pan to an exact point in the world
panTo :: Pos -> Camera -> Camera
panTo = set cameraPan

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
panTowards :: Pos -> Camera -> Camera
panTowards p c = panTo (closestPan p c) c

-- Return the position closest to the desired pan point
-- (I.E. x and y will either be as requested or the nearest border they are
-- past)
closestPan :: Pos -> Camera -> Pos 
closestPan (Pos (V2 x y)) c = Pos $ V2 (closestPanX x c) (closestPanY y c)

-- Return the X point closest to the desired pan point
-- (I.E. either the left or right boundary or the given point)
closestPanX :: CFloat -> Camera -> CFloat
closestPanX x c
  | x                      < c^.cameraBoundaryLeft = c^.cameraBoundaryLeft
  | c^.cameraBoundaryRight < x                     = c^.cameraBoundaryRight
  | otherwise                                      = x

-- Return the Y point closest to the desired pan point
-- (I.E. either the top or bottom boundary or the given point)
closestPanY :: CFloat -> Camera -> CFloat
closestPanY y c
  | y                     < c^.cameraBoundaryUp = c^.cameraBoundaryUp
  | c^.cameraBoundaryDown < y                   = c^.cameraBoundaryDown
  | otherwise = y



-- create a camera with:
-- - frame dimensions
-- - absolute boundaries
mkCamera :: Size -> V4 CFloat -> Maybe Camera
mkCamera dim boundaries = Just $ Camera (Pos $ V2 0 0) dim boundaries True

-- shoot a frame of the scene, the background, the subject, any actors and props adjusted
-- for the cameras pan
shoot :: Camera -> Renderer -> Stage -> IO ()
shoot c renderer stage = do
  clear renderer

  let unitSize      = stage^.stageBackground.backgroundTileGrid.tileGridUnitSize
      unitSizeF     = fromIntegral unitSize
      subjectHeight = stage^.stageSubject.thingTile.tileHeight

  -- Get the subject tile and attempt to pan to it
  let subjectTile = stage^.stageSubject.thingTile
  let (Pos (V2 subjectX subjectY)) = subjectTile^.tilePos
      c' = if c^.cameraTrackSubject
             then panLeftBy (3 * unitSizeF)
                  . panUpBy (c^.cameraHeight - ((3 * unitSizeF) + subjectHeight) )
                  . panTowards (Pos $ V2 subjectX subjectY) $ c
             else c


  -- render a possible background image
  case stage^.stageBackground.backgroundImage of
    Nothing                -> return ()
    Just backgroundTexture -> copy renderer backgroundTexture Nothing $ Just (Rectangle (P $ V2 0 0) (V2 (floor $ c'^.cameraWidth) (floor $ c'^.cameraHeight)))

  -- render the background that falls within the frame
  renderTileGrid (Pos $ V2 (c'^.cameraPanX) (c'^.cameraPanY))
                 (Size $ V2 (c'^.cameraWidth) (c'^.cameraHeight))
                 renderer
                 (stage^.stageBackground.backgroundTileGrid)

  -- render the subject within the frame
  renderTile renderer $ over tilePos (`worldToCamera` c') subjectTile

  -- render the 'Thing's
  -- TODO: MESS
  let reps :: [Reproducing Thing Subject ()]
      reps = map fst $ stage^.stageCollectReproducing.to collected

      lives :: [Live Thing Subject ([Reproducing Thing Subject ()],())]
      lives = toListOf (traverse.reproducing) $ reps

      things = map (`withLiveClient` _client) lives
  mapM_ (\thing -> renderTile renderer $ over tilePos (`worldToCamera` c') $ thing^.thingTile) things

  present renderer

-- Set the camera boundaries.
-- TODO: Consider what to do if the pan is outside the new boundaries
setBoundaries :: V4 CFloat -> Camera -> Camera
setBoundaries = set cameraBoundaries

