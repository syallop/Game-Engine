{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import SDL
import Linear (V2(..),V4(..))
import Linear.Affine (Point(..))
import Foreign.C.Types
import GHC.Word
import Control.Applicative

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.List
import Data.Text as T hiding (replicate,foldr,map,toLower,length)

import Game.Tile
import Game.Tiles
import Game.Thing
import Game.Stage
import Game.Background
import Game.Camera
import Game.Velocity
import Game.Force
import Game.TileConfigReader
import Game.ThingConfigReader
import Game.StageConfigReader

import Debug.Trace

data Game = Game
  {_quit     :: Bool

  ,_stage    :: Stage Text -- the current stage
  ,_stageIx  :: Int        -- the current stage ix in stages
  ,_stages   :: Stages     -- all the possible stages

  ,_camera   :: Camera
  ,_panSpeed :: CInt
  }
  deriving Show

initialGame :: Renderer -> CInt -> CInt -> IO Game
initialGame renderer width height = do
  let quit     = False

  -- Load a stage
  stages <- parseStages "R/Stages" renderer
  let stage0     = (!! 0) . M.elems $ stages

  -- Boundaries the camera should not move past
  let tileSize = stageUnitSize stage0
      rows     = (_tileRows . stageBackgroundTiles $ stage0)
      boundaryLeft   = 0
      boundaryRight  = tilesWidth rows * tileSize
      boundaryTop    = 0
      boundaryBottom = tilesHeight rows * tileSize

  --todo pan bottom edge
  let initialCamera  = panTo (V2 0 (backgroundHeight (stageBackground stage0) - height)) $ fromJust
                                   $ mkCamera (V2 width height)
                                              (V4 boundaryLeft boundaryRight boundaryTop boundaryBottom)

  return $ Game quit stage0 0 stages initialCamera 1

data Command
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown

  | PanLeft
  | PanRight
  | PanUp
  | PanDown

  | IncreasePan
  | DecreasePan

  | PrevStage
  | NextStage

  | TrackSubject

  | Jump
  | Shoot
  | Quit
  deriving (Show,Eq)

data TileType
  = Aperture
  | Black
  | Blue
  | EmptyTile
  | Green
  | MoneyBag
  | Red
  | White
  | YellowCircle
  deriving (Eq,Ord,Show,Enum)

-- Lookup a Texts corresponding TileType.
-- Partial. Assuming the TileType is named identically.
toTileType :: Text -> TileType
toTileType t = fromJust . lookup t . map (\tt -> (lowerCase . pack . show $ tt,tt)) $ enumFrom Aperture

-- Lower the case of the first character of some Text
lowerCase :: Text -> Text
lowerCase t = case uncons t of
  Nothing     -> t
  Just (c,cs) -> cons (toLower c) cs

pattern Floor     = Green
pattern Ceiling   = Blue
pattern WallLeft  = Blue
pattern WallRight = Black
pattern Air       = EmptyTile

-- Set up the window, etc and the initial game
initializeWindow :: CInt -> CInt -> IO (Window,Renderer)
initializeWindow width height = do
  HintRenderScaleQuality $= ScaleLinear
  do renderQuality <- get HintRenderScaleQuality
     when (renderQuality /= ScaleLinear) $
         putStrLn "Linear texture filtering not enabled!"

  window <- createWindow "Game" defaultWindow{windowInitialSize = V2 width height}
  showWindow window

  renderer <- createRenderer window (-1) $ RendererConfig{rendererType = AcceleratedRenderer
                                                         ,rendererTargetTexture = False
                                                         }
  rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  return (window,renderer)


main :: IO ()
main = do
  let w = 640
      h = 480
  (window,renderer) <- initializeWindow w h
  game <- initialGame renderer w h
  gameLoop (window,renderer) game

toCommand :: Event -> Maybe Command
toCommand event = case eventPayload event of

    KeyboardEvent keyboardEvent
        |  keyboardEventKeyMotion keyboardEvent == Pressed
         -> case keysymKeycode (keyboardEventKeysym keyboardEvent) of

               KeycodeLeft
                 -> Just PanLeft
               KeycodeRight
                 -> Just PanRight
               KeycodeUp
                 -> Just PanUp
               KeycodeDown
                 -> Just PanDown

               KeycodeW
                 -> Just MoveUp
               KeycodeS
                 -> Just MoveDown
               KeycodeA
                 -> Just MoveLeft
               KeycodeD
                 -> Just MoveRight

               KeycodeComma
                 -> Just DecreasePan
               KeycodePeriod
                 -> Just IncreasePan

               KeycodeZ
                 -> Just PrevStage
               KeycodeX
                 -> Just NextStage

               KeycodeT
                 -> Just TrackSubject

               KeycodeSpace
                 -> Just Jump

               KeycodeE
                 -> Just Shoot

               KeycodeQ
                 -> Just Quit
               _  -> Nothing
    _ -> Nothing

-- Update the Game state by the effect of a string of commands
runCommands :: Game -> [Command] -> Game
runCommands = foldr runCommand

-- Update the Game state by the effect of a single command
runCommand :: Command -> Game -> Game
runCommand c g = case c of
  MoveLeft
    -> g{_stage = applyForceSubject (Force $ V2 (-2) 0) $ _stage g}

  MoveRight
    -> g{_stage = applyForceSubject (Force $ V2 2 0) $ _stage g}

  MoveUp
    -> g{_stage = applyForceSubject (Force $ V2 (-1) 0) $ _stage g}

  MoveDown
    -> g{_stage = applyForceSubject (Force $ V2 1 0) $ _stage g}



  PanLeft
    -> g{_camera = panLeftBy (_panSpeed g) $ _camera g}

  PanRight
    -> g{_camera = panRightBy (_panSpeed g) $ _camera g}

  PanDown
    -> g{_camera = panDownBy (_panSpeed g) $ _camera g}

  PanUp
    -> g{_camera = panUpBy (_panSpeed g) $ _camera g}


  IncreasePan
    -> g{_panSpeed = _panSpeed g + 1}

  DecreasePan
    -> g{_panSpeed = if (_panSpeed g - 1) < 0 then 0 else _panSpeed g - 1}


  NextStage
    -> do let stageIx  = _stageIx g
              stageIx' = stageIx + 1
              stages   = _stages g
          maybe g (\nextStage -> g{_stage      = nextStage
                                  ,_stageIx    = stageIx'
                                  }
                  ) $ safeIndex (M.elems stages) stageIx'

  PrevStage
    -> do let stageIx  = _stageIx g
              stageIx' = stageIx - 1
              stages   = _stages g
          maybe g (\nextStage -> g{_stage      = nextStage
                                  ,_stageIx    = stageIx'
                                  }
                  ) $ safeIndex (M.elems stages) stageIx'

  Jump
    -> g{_stage = pushForceSubject (Force $ V2 0 (-15)) (_stage g)}


  TrackSubject
    -> let cam  = _camera g
           cam' = cam{_trackSubject = not . _trackSubject $ cam}
        in g{_camera = cam'}


  Shoot
    -> g

  Quit
    -> g{_quit = True}

-- Render a step of the game state
stepGame :: (Window,Renderer) -> Game -> IO (Bool,Game)
stepGame (window,renderer) game = if _quit game then return (True,game) else do
  -- Screen to white
  rendererDrawColor renderer $= white

  -- Update the stage
  let stage' = tickStage (_stage game)
      game'  = game{_stage = stage'}

  -- Shoot a frame of the game
  shoot (_camera game') renderer stage'

  return (False,game')


-- Main game loop
gameLoop :: (Window,Renderer) -> Game -> IO ()
gameLoop (window,renderer) game = do
  -- Parse events into Commands
  events <- pollEvents
  let commands :: [Command]
      commands = nub $ mapMaybe toCommand events

  -- calculate the next game state by the effect of all the commands
  let nextGame = runCommands game commands



  -- Render the new game state, which returns whether to quit
  (shouldQuit,nextGame') <- stepGame (window,renderer) nextGame
  (if shouldQuit then quitGame else gameLoop) (window,renderer) nextGame'

quitGame :: (Window,Renderer) -> Game -> IO ()
quitGame (window,renderer) g = do
  destroyRenderer renderer
  destroyWindow window
  quit

safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _ = Nothing
safeIndex (x:_)  0 = Just x
safeIndex (x:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n-1)

