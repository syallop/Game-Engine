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

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import Data.List
import Data.Text as T hiding (replicate,foldr,map,toLower)

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

import Debug.Trace

data Game t = Game
  {_quit   :: Bool
  ,_stage  :: Stage t
  ,_camera :: Camera
  }
  deriving Show

initialGame :: Renderer -> CInt -> CInt -> IO (Game TileType)
initialGame renderer width height = do
  -- Grab some textures..
  [ redTexture
   ,greenTexture
   ,blueTexture
   ,blackTexture
   ,whiteTexture
   ,playerTexture
   ,yellowCircleTexture
   ,moneyBagTexture
   ,forestTexture
   ] <- mapM (loadTexture renderer)
                   ["red.bmp"
                   ,"green.bmp"
                   ,"blue.bmp"
                   ,"black.bmp"
                   ,"white.bmp"
                   ,"playerT.bmp"
                   ,"yellowCircle.bmp"
                   ,"moneyBag.bmp"
                   ,"forest.bmp"
                   ]

  -- An example tileset with Text keys
  {-exTilesetText :: TileSet Text <- parseTileSet "R/Tilesets/ExampleTileset1" renderer-}
  exTilesetText :: TileSet Text <- parseTileSet "R/Tilesets/ExampleTileset2" renderer

  -- Convert the example tileset to TileType keys, under the assumption that TileType must
  -- have the same ordering and naming...
  let exTileset :: TileSet TileType
      exTileset = M.mapKeysMonotonic toTileType exTilesetText
  let tileSize = 64

      subjectTile = textureTile playerTexture (P $ V2 0 0) tileSize

      line       = Row $ WallLeft : (replicate 10 Air) ++ [WallRight]
      aboveBottomLine = Row $ WallLeft : Air : Air : Air : Air : Air      : Air : Air : Floor : Air : Air : WallRight : []
      bottomLine      = Row $ WallLeft : Air : Air : Air : Air : WallLeft : Air : Air : Floor : Air : Air : WallRight : []
      tileRows = Rows $ (replicate 5 line) ++ [aboveBottomLine,bottomLine] ++ [Row $ replicate 12 Floor]
      exampleTiles = fromJust $ mkTiles tileRows exTileset tileSize


  let quit = False

  -- Boundaries the camera should not move past
  let boundaryLeft   = 0
      boundaryRight  = tilesWidth tileRows * tileSize
      boundaryTop    = 0
      boundaryBottom = tilesHeight tileRows * tileSize

  -- all the things!
  things <- parseThings "R/Things" exTilesetText tileSize
  let coinThing   = fromJust . M.lookup "coin"  $ things
      playerThing = fromJust . M.lookup "player" $ things

  -- Specific instances of the things
  let fallingCoin0  = setMass . moveThingBy (V2 192 256) $ coinThing
      floatingCoin0 = moveThingBy (V2 128 128) coinThing
      player0       = moveThingBy (V2 tileSize tileSize) playerThing

  let background = fromJust $ mkBackground exampleTiles (Just forestTexture)
      {-subject    = Thing (moveR tileSize $ moveD (tileSize * 1) $ subjectTile) True True (Velocity $ V2 0 0)-}
      gravity    = Force $ V2 0 1
      stage      = fromJust $ setStage background player0 [fallingCoin0,floatingCoin0] gravity

  --todo pan bottom edge
  let initialCamera  = panTo (V2 0 (backgroundHeight background - height)) $ fromJust
                                   $ mkCamera (V2 width height)
                                              (V4 boundaryLeft boundaryRight boundaryTop boundaryBottom)

  return $ Game quit stage initialCamera

data Command
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown

  | PanLeft
  | PanRight
  | PanUp
  | PanDown

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
pattern Air      = EmptyTile

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

               KeycodeLeft  -> Just PanLeft
               KeycodeRight -> Just PanRight
               KeycodeUp    -> Just PanUp
               KeycodeDown  -> Just PanDown

               KeycodeW     -> Just MoveUp
               KeycodeS     -> Just MoveDown
               KeycodeA     -> Just MoveLeft
               KeycodeD     -> Just MoveRight

               KeycodeT     -> Just TrackSubject

               KeycodeSpace -> Just Jump
               KeycodeE     -> Just Shoot
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
    -> g{_stage = fromMaybe (_stage g) $ moveSubjectLeft $ _stage g}

  MoveRight
    -> g{_stage = fromMaybe (_stage g) $ moveSubjectRight $ _stage g}

  MoveUp
    -> g{_stage = fromMaybe (_stage g) $ moveSubjectUp $ _stage g}

  MoveDown
    -> g{_stage = fromMaybe (_stage g) $ moveSubjectDown $ _stage g}

  PanLeft
    -> g{_camera = panLeft $ _camera g}

  PanRight
    -> g{_camera = panRight $ _camera g}

  PanDown
    -> g{_camera = panDown $ _camera g}

  PanUp
    -> g{_camera = panUp $ _camera g}

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
stepGame :: (Show t,Ord t) => (Window,Renderer) -> Game t -> IO (Bool,Game t)
stepGame (window,renderer) game = if _quit game then return (True,game) else do
  -- Screen to white
  rendererDrawColor renderer $= white
  let stage' = tickStage (_stage game)
      game'  = game{_stage = stage'}

  -- Shoot a frame of the game
  shoot (_camera game') renderer stage'

  return (False,game')


-- Main game loop
gameLoop :: Show t => Ord t => (Window,Renderer) -> Game t -> IO ()
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

quitGame :: Ord t => (Window,Renderer) -> Game t -> IO ()
quitGame (window,renderer) g = do
  destroyRenderer renderer
  destroyWindow window
  quit

