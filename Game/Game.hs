{-# LANGUAGE
    OverloadedStrings
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  #-}
module Main where

import Control.Applicative
import Control.Lens hiding (cons,uncons)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Text as T hiding (replicate,foldr,map,toLower,length)
import Foreign.C.Types
import GHC.Word
import Linear (V2(..),V4(..))
import Linear.Affine (Point(..))
import SDL
import qualified Data.Map as M

import Game.Background
import Game.Camera
import Game.Force
import Game.Stage
import Game.StageConfigReader
import Game.Thing
import Game.ThingConfigReader
import Game.Tile
import Game.TileConfigReader
import Game.TileGrid
import Game.TileSet
import Game.Velocity

import Debug.Trace

data Game = Game
  {_gameQuit      :: Bool

  ,_gameStage     :: Stage  -- the current stage
  ,_gameStageIx   :: Int    -- the current stage ix in stages
  ,_gameStages    :: Stages -- all the possible stages

  ,_gameCamera    :: Camera
  ,_gamePanSpeed  :: CInt

  ,_gameLastTicks :: Word32 -- Number of ticks since sdl initialisation, as of the last game step
  ,_gameTickDelta :: CInt   -- Number of tenths of a tick since last check
  }
  deriving Show

makeLenses ''Game

initialGame :: Renderer -> CInt -> CInt -> IO Game
initialGame renderer frameWidth frameHeight = do
  let quit     = False

  -- Load a stage
  stages <- parseStages "R/Stages" renderer
  let stage0     = (!! 0) . M.elems $ stages

  -- Boundaries the camera should not move past
  let tileGrid = stage0^.stageBackground.backgroundTileGrid
      boundaryLeft   = 0
      boundaryRight  = tileGridWidth tileGrid
      boundaryTop    = 0
      boundaryBottom = tileGridHeight tileGrid

  --todo pan bottom edge
  let panSubjectTL   = panTo (P $ V2 0 (tileGridHeight tileGrid - frameHeight))
  let initialCamera  = panSubjectTL $ fromJust
                                    $ mkCamera (V2 frameWidth frameHeight)
                                               (V4 boundaryLeft boundaryRight boundaryTop boundaryBottom)

  return $ Game quit stage0 0 stages initialCamera 1 0 1

-- Infer the boundaries of the current stage (as the most exteme edges of the background tiles)
-- and set it.
inferCameraBoundaries :: Game -> Game
inferCameraBoundaries g =
  let tileGrid = g^.gameStage.stageBackground.backgroundTileGrid
      l = 0
      r = tileGridWidth tileGrid
      u = 0
      d = tileGridHeight tileGrid
      boundaries = V4 l r u d
    in over gameCamera (setBoundaries boundaries) g


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

-- Lower the case of the first character of some Text
lowerCase :: Text -> Text
lowerCase t = case uncons t of
  Nothing     -> t
  Just (c,cs) -> cons (toLower c) cs

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

-- Update the Game state by the effect of a string of commands
runCommands :: Game -> [Command] -> Game
runCommands = foldr runCommand

-- Update the Game state by the effect of a single command
runCommand :: Command -> Game -> Game
runCommand c g = case c of
  MoveLeft
    -> over gameStage (applyForceSubject (Force $ V2 (-4) 0)) g

  MoveRight
    -> over gameStage (applyForceSubject (Force $ V2 4 0)) g

  MoveUp
    -> over gameStage (applyForceSubject (Force $ V2 0 0)) g

  MoveDown
    -> over gameStage (applyForceSubject (Force $ V2 0 0)) g


  PanLeft
    -> over gameCamera (panLeftBy (g^.gamePanSpeed)) g

  PanRight
    -> over gameCamera (panRightBy (g^.gamePanSpeed)) g

  PanDown
    -> over gameCamera (panDownBy (g^.gamePanSpeed)) g

  PanUp
    -> over gameCamera (panUpBy (g^.gamePanSpeed)) g


  IncreasePan
    -> over gamePanSpeed (+1) g

  DecreasePan
    -> over gamePanSpeed (subtract 1) g

  PrevStage
    -> let stageIx    = g^.gameStageIx
           stageIx'   = stageIx - 1
           stages     = g^.gameStages
           mNextStage = safeIndex (M.elems stages) stageIx'
          in inferCameraBoundaries $ maybe g (\nextStage -> set gameStage nextStage . set gameStageIx stageIx' $ g) mNextStage

  NextStage
    -> let stageIx    = g^.gameStageIx
           stageIx'   = stageIx + 1
           stages     = g^.gameStages
           mNextStage = safeIndex (M.elems stages) stageIx'
          in inferCameraBoundaries $ maybe g (\nextStage -> set gameStage nextStage . set gameStageIx stageIx' $ g) mNextStage


  Jump
    -> over gameStage (pushForceSubject (Force $ V2 0 (-12))) g

  TrackSubject
    -> let cam  = g^.gameCamera
           cam' = over cameraTrackSubject not cam
        in set gameCamera cam' g

  Shoot
    -> g

  Quit
    -> set gameQuit True g

-- Render a step of the game state
stepGame :: (Window,Renderer) -> Game -> IO (Bool,Game)
stepGame (window,renderer) game = if game^.gameQuit then return (True,game) else do
  -- Screen to white
  rendererDrawColor renderer $= white

  -- Update the stage
  let game' = set gameStage (tickStage (game^.gameTickDelta) (game^.gameStage)) game

  -- Shoot a frame of the game
  shoot (game'^.gameCamera) renderer (game'^.gameStage)

  return (False,game')


-- Main game loop
gameLoop :: (Window,Renderer) -> Game -> IO ()
gameLoop (window,renderer) game0 = do
  -- Get commands as the result of keydowns
  commands <- keyboardCommands
  game1 <- tickDelta game0

  -- calculate the next game state by the effect of all the commands
  let game2 = runCommands game1 commands

  -- Render the new game state, which returns whether to quit
  (shouldQuit,game3) <- stepGame (window,renderer) game2
  (if shouldQuit then quitGame else gameLoop) (window,renderer) game3

-- Commands to be issued as long as a key is still down
keydownCommands :: M.Map Scancode Command
keydownCommands = M.fromList
  [(ScancodeLeft ,PanLeft)
  ,(ScancodeRight,PanRight)
  ,(ScancodeUp   ,PanUp)
  ,(ScancodeDown ,PanDown)

  ,(ScancodeW,MoveUp)
  ,(ScancodeS,MoveDown)
  ,(ScancodeA,MoveLeft)
  ,(ScancodeD,MoveRight)

  ,(ScancodeE ,Shoot)
  ]

-- Commands to be issued when a key is pressed
keypressCommands :: M.Map Keycode Command
keypressCommands = M.fromList
  [(KeycodeZ,PrevStage)
  ,(KeycodeX,NextStage)

  ,(KeycodeComma ,DecreasePan)
  ,(KeycodePeriod,IncreasePan)

  ,(KeycodeSpace,Jump)

  ,(KeycodeQ,Quit)

  ,(KeycodeT,TrackSubject)
  ]

keyboardCommands :: IO [Command]
keyboardCommands = do
  pumpEvents
  f <- getKeyboardState
  let heldCommands = mapMaybe (\scancode -> if f scancode then M.lookup scancode keydownCommands else Nothing) $ M.keys keydownCommands

  events <- pollEvents
  let downCommands = mapMaybe (\event -> case eventPayload event of
                                           KeyboardEvent kEv
                                             | (keyboardEventKeyMotion kEv == Pressed) && (keyboardEventRepeat kEv == False)
                                              ->  M.lookup (keysymKeycode . keyboardEventKeysym $ kEv) keypressCommands

                                           _ -> Nothing
                              )
                              events

  return $ nub $ heldCommands ++ downCommands


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

tickDelta :: Game -> IO Game
tickDelta g = do
  total <- ticks
  let last  = g^.gameLastTicks
      delta = total - last
  return $ g{_gameLastTicks = total
            ,_gameTickDelta = word32ToCInt delta `div` 10
            }

word32ToCInt :: Word32 -> CInt
word32ToCInt = toEnum . fromEnum

