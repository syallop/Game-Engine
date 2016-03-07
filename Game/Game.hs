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
import Game.Camera

data Game t = Game
  {_quit         :: Bool
  ,_camera       :: Camera t
  }

initialGame :: Game TileType
initialGame = Game quit camera
  where
    camera :: Camera TileType
    camera = panBottomEdge $ fromJust $ mkCamera (V2 width height)
                                                 (V4 0 (tilesWidth tileColumns) (tilesHeight tileColumns) 0)
                                                 tiles
                                                 subject

    -- width and height of the screen/ camera
    width  = 640
    height = 480

    -- whether to quit
    quit   = False

    -- 'the player'
    subject = Subject $ moveR tileSize $ moveD (height - (2* tileSize)) $ tile red (P $ V2 0 0) tileSize

    -- unit radius of all tiles
    tileSize = 32 -- radius of tiles

    -- The specific configuration of tiles
    tiles = fromJust $ mkTiles tileColumns tileset tileSize

    -- Columns of rows of tiletypes
    tileColumns = TileColumn $ (replicate 23 line) ++ [TileRow $ replicate 32 Floor]
      where line = TileRow $ WallLeft : (replicate 30 Air) ++ [WallRight]

    -- Map each tile type to info describing it
    tileset = M.fromList
      [(Floor,TileInfo green)
      ,(Ceiling,TileInfo blue)
      ,(WallLeft,TileInfo blue)
      ,(WallRight,TileInfo black)
      ,(Air,TileInfo white)
      ]

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
  deriving (Eq,Ord)

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
  let game = initialGame
  (window,renderer) <- initializeWindow (width . _camera $ game) (height . _camera $ game)
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
runCommands :: Game t -> [Command] -> Game t
runCommands = foldr runCommand

-- Update the Game state by the effect of a single command
runCommand :: Command -> Game t -> Game t
runCommand c g = case c of
  MoveLeft
    -> g{_camera = moveSubjectLeft $ _camera g}

  MoveRight
    -> g{_camera = moveSubjectRight $ _camera g}

  MoveUp
    -> g

  MoveDown
    -> g

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
  shoot (_camera game) renderer

  return False


-- Main game loop
gameLoop :: Ord t => (Window,Renderer) -> Game t -> IO ()
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
