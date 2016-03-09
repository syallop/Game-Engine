{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import SDL
import Linear (V2(..),V4(..))
import Linear.Affine (Point(..))
import Foreign.C.Types
import GHC.Word

import qualified Data.Map as M
import Data.Maybe

import Game.Tile
import Game.Tiles
import Game.Stage
import Game.Camera

data Game t = Game
  {_quit   :: Bool
  ,_stage  :: Stage t
  ,_camera :: Camera
  }

initialGame :: Renderer -> CInt -> CInt -> IO (Game TileType)
initialGame renderer width height = do
  -- Grab some textures..
  [ redTexture
   ,greenTexture
   ,blueTexture
   ,blackTexture
   ,whiteTexture
   ,playerTexture
   ] <- mapM (loadTexture renderer)
                   ["red.bmp"
                   ,"green.bmp"
                   ,"blue.bmp"
                   ,"black.bmp"
                   ,"white.bmp"
                   ,"playerT.bmp"
                   ]

  -- Map each tile to info describing it
  let tileSet :: M.Map TileType TileInfo
      tileSet = M.fromList [(Floor    ,InfoTextured greenTexture True)
                           ,(Ceiling  ,InfoTextured blueTexture  True)
                           ,(WallLeft ,InfoTextured blueTexture  True)
                           ,(WallRight,InfoTextured blackTexture True)
                           ,(Air      ,InfoTextured whiteTexture False)
                           ]
      tileSize = 64 

      subjectTile = textureTile playerTexture (P $ V2 0 0) tileSize

      line       = Row $ WallLeft : (replicate 10 Air) ++ [WallRight]
      aboveBottomLine = Row $ WallLeft : Air : Air : Air : Air : Air      : Air : Air : Floor : Air : Air : WallRight : [] 
      bottomLine      = Row $ WallLeft : Air : Air : Air : Air : WallLeft : Air : Air : Floor : Air : Air : WallRight : [] 
      tileRows = Rows $ (replicate 5 line) ++ [aboveBottomLine,bottomLine] ++ [Row $ replicate 12 Floor]
      exampleTiles = fromJust $ mkTiles tileRows tileSet tileSize


  let quit = False

  let boundaryLeft   = 0
      boundaryRight  = (tilesWidth tileRows) * tileSize
      boundaryTop    = 0
      boundaryBottom = (tilesHeight tileRows) * tileSize

  let background = Background exampleTiles
      subject    = Subject $ moveR tileSize $ moveD (tileSize * 6) $ subjectTile
      stage      = fromJust $ setStage background subject 

  --todo pan bottom edge
  let initialCamera  = fromJust $ mkCamera (V2 width height)
                                           (V4 boundaryLeft boundaryRight boundaryTop boundaryBottom)

  return $ Game quit stage initialCamera 

data Command
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Shoot
  | Quit

data TileType
  = Floor
  | Ceiling
  | WallLeft
  | WallRight
  | Air
  deriving (Eq,Ord,Show)

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
               KeycodeLeft  -> Just MoveLeft
               KeycodeRight -> Just MoveRight
               KeycodeUp    -> Just MoveUp
               KeycodeDown  -> Just MoveDown
               KeycodeSpace -> Just Shoot
               KeycodeQ     -> Just Quit
               _  -> Nothing
    _ -> Nothing

-- Update the Game state by the effect of a string of commands
runCommands :: Show t => Ord t => Game t -> [Command] -> Game t
runCommands = foldr runCommand

-- Update the Game state by the effect of a single command
runCommand :: Show t => Ord t => Command -> Game t -> Game t
runCommand c g = case c of
  MoveLeft
    -> g{_stage = moveSubjectLeft $ _stage g} 

  MoveRight
    -> g{_stage = moveSubjectRight $ _stage g}

  MoveUp
    -> g{_stage = moveSubjectUp $ _stage g}

  MoveDown
    -> g{_stage = moveSubjectDown $ _stage g}

  Shoot
    -> g

  Quit
    -> g{_quit = True}

-- Render a step of the game state
stepGame :: Ord t => (Window,Renderer) -> Game t -> IO Bool
stepGame (window,renderer) game = if _quit game then return True else do
  -- Screen to white
  rendererDrawColor renderer $= white

  -- Shoot a frame of the game
  shoot (_camera game) renderer (_stage game)

  return False


-- Main game loop
gameLoop :: Show t => Ord t => (Window,Renderer) -> Game t -> IO ()
gameLoop (window,renderer) game = do
  -- Parse events into Commands
  events <- pollEvents
  let commands :: [Command]
      commands = catMaybes $ map toCommand events

  -- calculate the next game state by the effect of all the commands
  let nextGame = runCommands game commands

  -- Render the new game state, which returns whether to quit
  shouldQuit <- stepGame (window,renderer) nextGame
  (if shouldQuit then quitGame else gameLoop) (window,renderer) nextGame 

quitGame :: Ord t => (Window,Renderer) -> Game t -> IO ()
quitGame (window,renderer) g = do
  destroyRenderer renderer
  destroyWindow window
  quit

{-loadSurface :: Surface -> FilePath -> IO Surface-}
{-loadSurface screenSurface file = do-}
  {-return undefined -}
