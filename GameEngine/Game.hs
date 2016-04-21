{-# LANGUAGE
    OverloadedStrings
  , PatternSynonyms
  , ScopedTypeVariables
  , TemplateHaskell
  #-}
module Main where

import Data.Foldable (foldrM)
import Control.Applicative
import Control.Lens hiding (cons,uncons)
import Control.Monad
import Data.Char
import Data.List hiding (uncons)
import Data.Maybe
import Data.Text as T hiding (replicate,foldr,map,toLower,length,null,drop)
import Foreign.C.Types
import GHC.Word
import Linear (V2(..),V4(..))
import Linear.Affine (Point(..),lensP)
import SDL
import SDL.Raw (Color(..))
import qualified Data.Map as M

import GameEngine

import Debug.Trace

-- Game state and configuration options
data Game = Game
  {_gameQuit      :: Bool

  ,_gameStage     :: Stage  -- the current stage
  ,_gameStageIx   :: Int    -- the current stage ix in stages
  ,_gameStages    :: Stages -- all the possible stages

  ,_gameCamera    :: Camera
  ,_gamePanSpeed  :: CFloat

  ,_gameLastTicks :: Word32 -- Number of ticks since initialisation.
  ,_gameTickDelta :: CInt   -- Number of ticks since last check.

  ,_gameFacingRight :: Bool -- Is the player "facing" right?

  ,_gameFont :: TTFFont     -- Font used to draw game txts
  ,_gameTxts :: Collect Txt -- Collection of txt the game draws
  }
  deriving Show
makeLenses ''Game



-- Infer the boundaries of the current stage (as the most extreme edges of the background tiles)
-- and set it.
inferCameraBoundaries :: Game -> Game
inferCameraBoundaries g =
  let tileGrid = g^.gameStage.stageBackground.backgroundTileGrid
      l = 0
      r = tileGridWidth tileGrid
      u = 0
      d = tileGridHeight tileGrid
      boundaries = V4 l (fromIntegral r) u (fromIntegral d)
    in over gameCamera (setBoundaries boundaries) g

-- Set the "stageName" txt to the name of the current stage
setStageName :: Renderer -> Game -> IO Game
setStageName renderer g = do
    -- Set a title for the stage
  let stageNames = g^.gameStages.to M.keys
      stageName  = stageNames !! (g^.gameStageIx)
  stageNameTxt <- mkTxt stageName (Color 112 11 20 maxBound) (g^.gameFont) (Rectangle (P $ V2 0 0) (V2 250 20)) renderer
  return $ over gameTxts (fst . insertNamed "stageName" stageNameTxt . deleteName "stageName") g



-- Execute the game as defined by "R/" directory with a screen size of 640*480
main :: IO ()
main = do
  let w = 640
      h = 480
  (window,renderer) <- initializeWindow w h
  game              <- initialGame renderer (fromIntegral w) (fromIntegral h)
  gameLoop (window,renderer) game

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



-- An initial Game where resources are loaded from "R/".
initialGame :: Renderer -> CFloat -> CFloat -> IO Game
initialGame renderer frameWidth frameHeight = do
  let quit     = False

  let agents = mkCollect [(movingAgent, "movingAgent"),(shootingAgent, "shootingAgent")] []

  -- Load a stage
  stages <- parseStages agents "R/Stages" renderer
  let stage0     = (!! 0) . M.elems $ stages

  -- Boundaries the camera should not move past
  let tileGrid = stage0^.stageBackground.backgroundTileGrid
      boundaryLeft   = 0
      boundaryRight  = fromIntegral . tileGridWidth $ tileGrid
      boundaryTop    = 0
      boundaryBottom = fromIntegral . tileGridHeight $ tileGrid

  --todo pan bottom edge
  let panSubjectTL   = panTo (Pos $ V2 0 (fromIntegral (tileGridHeight tileGrid) - frameHeight))
  let initialCamera  = panSubjectTL $ fromJust
                                    $ mkCamera (Size $ V2 frameWidth frameHeight)
                                               (V4 boundaryLeft boundaryRight boundaryTop boundaryBottom)

  --Initialise font subsystem
  initText
  ubuntuMonoTTF <- openFont "R/Fonts/ExampleFonts/ubuntu-mono/UbuntuMono-R.ttf" 22

  -- Set a title for the stage
  setStageName renderer . Game quit stage0 0 stages initialCamera 1 0 1 True ubuntuMonoTTF . collect $ []

  where
    movingAgent :: Agent (Subject,Thing) Text
    movingAgent = mkAgent initialState actF
      where
        initialState = ()

        actF :: (Subject,Thing) -> () -> (Text,())
        actF (subject,selfThing) st
          -- subject left => walk left
          | subject^.thingTile.tilePosX < selfThing^.thingTile.tilePosX
           = ("walkleft",st)

          -- we're left => walk right
          | selfThing^.thingTile.tilePosX < subject^.thingTile.tilePosX
           = ("walkright",st)

          | otherwise
           = ("",st)

    shootingAgent :: Agent (Subject,Thing) Text
    shootingAgent = mkAgent reloadRate actF
      where
        reloadRate :: Int
        reloadRate = 5

        actF :: (Subject,Thing) -> Int -> (Text,Int)
        actF (subject,selfThing) st

          -- enough time has passed to shoot, subject is left => shootleft
          | st == 0 && subject^.thingTile.tilePosX < selfThing^.thingTile.tilePosX
           = ("shootleft",5)

        -- enough time has passed to shoot, subject is right => shootright
          | st == 0 && selfThing^.thingTile.tilePosX < subject^.thingTile.tilePosX
           = ("shootright",5)

          -- subject left => walk left
          | subject^.thingTile.tilePosX < selfThing^.thingTile.tilePosX
           = ("walkleft",max 0 $ st-1)

          -- we're left => walk right
          | selfThing^.thingTile.tilePosX < subject^.thingTile.tilePosX
           = ("walkright",max 0 $ st-1)

          | otherwise
           = ("",max 0 $ st-1)


-- Main game loop
gameLoop :: (Window,Renderer) -> Game -> IO ()
gameLoop (window,renderer) game0 = do
  -- Get commands as the result of keydowns
  commands <- keyboardCommands
  game1    <- updateTicks game0

  -- calculate the next game state by the effect of all the commands
  game2 <- runCommands renderer game1 commands

  -- Update the stage
  -- Quit when the player reaches 0 health
  let newStage   = tickStage (game2^.gameTickDelta) (game2^.gameStage)
      shouldQuit = newStage^.stageSubject.thingHealth.to atMin
      game3      = set gameStage newStage game2

  -- Render the new game state, which returns whether to quit
  game4 <- renderGame (window,renderer) game3

  -- If the user asked to quit, or the player health is 0, quit.
  -- If the collection of opposing things is empty, progress to the next stage
  let nextLoop = if shouldQuit || game4^.gameQuit
                   then quitGame
                   else if game4^.gameStage.to ((== 0) . remainingConsumable)
                          then if null . drop (game4^.gameStageIx + 1) $ (M.keys $ game4^.gameStages)
                                 then winGame -- last level over
                                 else \(w,r) g -> do g' <- nextStage r g
                                                     gameLoop (w,r) g' -- next level
                          else gameLoop -- level not over
  nextLoop (window,renderer) game4

-- Render a step of the game state
renderGame :: (Window,Renderer) -> Game -> IO Game
renderGame (window,renderer) game = do
  -- Shoot a frame of the game
  shoot (game^.gameCamera) renderer (game^.gameStage) (game^.gameTxts.to (map fst . collected))
  return game


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


-- Lower the case of the first character of some Text
lowerCase :: Text -> Text
lowerCase t = case uncons t of
  Nothing     -> t
  Just (c,cs) -> cons (toLower c) cs

-- Update the Game state by the effect of a string of commands
runCommands :: Renderer -> Game -> [Command] -> IO Game
runCommands renderer = foldrM (runCommand renderer)

-- Update the Game state by the effect of a single command
runCommand :: Renderer -> Command -> Game -> IO Game
runCommand renderer c g = case c of
  MoveLeft
    -> return
     . over gameStage (applyForceSubject (Force $ V2 (-4) 0))
     . set gameFacingRight False
     $ g

  MoveRight
    -> return
     . over gameStage (applyForceSubject (Force $ V2 4 0))
     . set gameFacingRight True
     $ g

  MoveUp
    -> return . over gameStage (applyForceSubject (Force $ V2 0 0)) $ g

  MoveDown
    -> return . over gameStage (applyForceSubject (Force $ V2 0 0)) $ g


  PanLeft
    -> return . over gameCamera (panLeftBy (g^.gamePanSpeed)) $ g

  PanRight
    -> return . over gameCamera (panRightBy (g^.gamePanSpeed)) $ g

  PanDown
    -> return . over gameCamera (panDownBy (g^.gamePanSpeed)) $ g

  PanUp
    -> return . over gameCamera (panUpBy (g^.gamePanSpeed)) $ g


  IncreasePan
    -> return . over gamePanSpeed (+1) $ g

  DecreasePan
    -> return . over gamePanSpeed (subtract 1) $ g

  PrevStage
    -> prevStage renderer g

  NextStage
    -> nextStage renderer g

  Jump
    -> return . over gameStage (pushForceSubject (Force $ V2 0 (-10))) $ g

  TrackSubject
    -> let cam  = g^.gameCamera
           cam' = over cameraTrackSubject not cam
        in return . set gameCamera cam' $ g

  Shoot
    -> let xVel = 2 * if g^.gameFacingRight then 1 else -1
           b = bullet xVel (g^.gameStage.stageSubject)
          in return . over gameStage (addUs Nothing b) $ g

  Quit
    -> return . set gameQuit True $ g

nextStage :: Renderer -> Game -> IO Game
nextStage renderer g =
  let stageIx    = g^.gameStageIx
      stageIx'   = stageIx + 1
      stages     = g^.gameStages
      mNextStage = safeIndex (M.elems stages) stageIx'
     in setStageName renderer . inferCameraBoundaries . maybe g (\nextStage -> set gameStage nextStage . set gameStageIx stageIx' $ g) $ mNextStage

prevStage :: Renderer -> Game -> IO Game
prevStage renderer g =
  let stageIx    = g^.gameStageIx
      stageIx'   = stageIx - 1
      stages     = g^.gameStages
      mNextStage = safeIndex (M.elems stages) stageIx'
     in setStageName renderer . inferCameraBoundaries . maybe g (\nextStage -> set gameStage nextStage . set gameStageIx stageIx' $ g) $ mNextStage



quitGame :: (Window,Renderer) -> Game -> IO ()
quitGame (window,renderer) g = do
  destroyRenderer renderer
  destroyWindow window
  quitText
  quit

winGame :: (Window,Renderer) -> Game -> IO ()
winGame (window,renderer) g = do
  putStrLn "Win!"
  quitGame (window,renderer) g

safeIndex :: [a] -> Int -> Maybe a
safeIndex []     _ = Nothing
safeIndex (x:_)  0 = Just x
safeIndex (x:xs) n
  | n < 0     = Nothing
  | otherwise = safeIndex xs (n-1)

-- Update the games knowledge of:
-- - The total ticks since SDL was initialised
-- - How many ticks have occured since the last call to this update function
updateTicks :: Game -> IO Game
updateTicks g = do
  total <- ticks
  let last  = g^.gameLastTicks
      delta = total - last
  return $ g{_gameLastTicks = total
            ,_gameTickDelta = word32ToCInt delta
            }

word32ToCInt :: Word32 -> CInt
word32ToCInt = toEnum . fromEnum


