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

data Game t = Game
  {_playerTile   :: Tile
  ,_health       :: CInt
  ,_quit         :: Bool
  ,_screenWidth  :: CInt
  ,_screenHeight :: CInt
  ,_tiles        :: Tiles t

  -- 'camera' offset.
  ,_panX         :: CInt
  ,_panY         :: CInt
  }

initialGame :: Game TileType
initialGame = Game playerTile health quit width height tiles panX panY
  where
    width  = 640 -- screen/ camera width
    height = 480 -- screen/ camera height
    health = 50
    quit   = False
    playerTile = setRadius tileSize $
                 moveR tileSize $                -- one tile right from left
                 moveD (height - (2*tileSize)) $ -- two tiles up from bottom
                 setColor red $ defaultTile

    tileSize = 32 -- radius of tiles

    tiles = fromJust $ mkTiles tileColumns tileset tileSize
    tileColumns = TileColumn $ (replicate 23 line) ++ [TileRow $ replicate 32 Floor]
    line = TileRow $ WallLeft : (replicate 30 Air) ++ [WallRight]
    tileset = M.fromList
      [(Floor,TileInfo green)
      ,(Ceiling,TileInfo blue)
      ,(WallLeft,TileInfo blue)
      ,(WallRight,TileInfo black)
      ,(Air,TileInfo white)
      ]

    panX = 0

    -- Initially pan down to the bottom of the tileSet, and then up a tile
    panY = (-1 * ((tileSetHeight * tileSize) - height))

    tileSetHeight = toEnum . length . _tileColumn $ tileColumns

data Command
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | Shoot
  | Quit

-- Todo: Some form of collision detection instead of always panning
canPanRight :: Game t -> Bool
canPanRight game = True

canPanLeft :: Game t -> Bool
canPanLeft game = True

canPanUp :: Game t -> Bool
canPanUp game = True

canPanDown :: Game t -> Bool
canPanDown game = True

moveRight :: Game t -> Game t
moveRight game =
  -- If the camera can pan right without going 'too far'
  -- pan the camera. otherwise, move the player within the camera
  if canPanRight game
    then game{_panX = (_panX game) - 1}
    else game{_playerTile = moveR 1 $ _playerTile game}

moveLeft :: Game t -> Game t
moveLeft game =
  -- If the camera can pan left without going 'too far'
  -- pan the camera. otherwise, move the player within the camera
  if canPanLeft game
    then game{_panX = (_panX game) + 1}
    else game{_playerTile = moveL 1 $ _playerTile game}

moveDown :: Game t -> Game t
moveDown game =
  -- If the camera can pan down without going 'too far'
  -- pan the camera. otherwise, move the player within the camera
  if canPanDown game
    then game{_panY = (_panY game) - 1}
    else game{_playerTile = moveD 1 $ _playerTile game}

moveUp :: Game t -> Game t
moveUp game =
  -- If the camera can pan up without going 'too far'
  -- pan the camera. otherwise, move the player within the camera
  if canPanUp game
    then game{_panY = (_panY game) + 1}
    else game{_playerTile = moveU 1 $ _playerTile game}


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
  (window,renderer) <- initializeWindow (_screenWidth game) (_screenHeight game)
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
    -> moveLeft g

  MoveRight
    -> moveRight g

  MoveUp
    -> moveUp g

  MoveDown
    -> moveDown g

  Shoot
    -> g

  Quit
    -> g{_quit = True}

-- Render a step of the game state
stepGame :: Ord t => (Window,Renderer) -> Game t -> IO Bool
stepGame (window,renderer) game = if _quit game then return True else do

  -- Screen to white
  rendererDrawColor renderer $= white
  clear renderer

  renderTiles (V2 (_panX game) (_panY game))
              (_screenWidth game,_screenHeight game)
              renderer
              (_tiles game)

  -- Draw 'player'
  drawPlayer renderer game

  present renderer
  return False

drawPlayer :: Renderer -> Game t -> IO ()
drawPlayer renderer game = drawTile renderer (_playerTile game)



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
