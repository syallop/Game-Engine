{-# LANGUAGE TemplateHaskell #-}
-- Defines individual square tiles in colored and textured varieties.
-- A tile has a position and a radius which can be queried and altered.
-- A tile can be rendered with 'renderTile'
module GameEngine.Tile
  (TileColor
  ,redC,greenC,blueC,alphaC
  ,white,black,red,green,blue

  ,TileType(..)
  ,tileTypeColor
  ,tileTypeIsSolid
  ,tileTypeIsClimbable
  ,tileTypeTexture
  ,loadTileTypeTextured

  ,Tile()
  ,mkTile
  ,defaultTile

  ,tilePos
  ,tilePosX
  ,tilePosY
  ,tileSize
  ,tileWidth
  ,tileHeight
  ,tileRectangle

  ,tileL
  ,tileR
  ,tileT
  ,tileB

  ,tileTL
  ,tileTR
  ,tileBL
  ,tileBR

  ,moveTile
  ,moveTileR
  ,moveTileL
  ,moveTileD
  ,moveTileU

  ,renderTile
  ,loadTexture
  ,tileToHitBox
  )
  where

import GameEngine.HitBox
import GameEngine.Position
import GameEngine.Size

import Control.Applicative
import Control.Lens
import Foreign.C.Types
import GHC.Word
import Linear
import Linear.Affine
import SDL

-- Alias for a vector of RGBA
type TileColor = V4 Word8

redC, greenC, blueC, alphaC :: Lens' TileColor Word8
redC   = _x
greenC = _y
blueC  = _z
alphaC = _w

-- TileColors for convenience
white,black,red,green,blue :: TileColor
white = V4 maxBound maxBound maxBound maxBound
black = V4 minBound minBound minBound maxBound
red   = V4 maxBound minBound minBound maxBound
green = V4 minBound maxBound minBound maxBound
blue  = V4 minBound minBound maxBound maxBound

instance Show Texture where
  show _ = "TEXTURE"

data TileType
  = TileTypeColored
    {_tileTypeColor       :: TileColor
    ,_tileTypeIsSolid     :: Bool
    ,_tileTypeIsClimbable :: Bool
    }

  | TileTypeTextured
    {_tileTypeTexture :: Texture
    ,_tileTypeIsSolid :: Bool
    ,_tileTypeIsClimbable :: Bool
    }

  | TileTypeInvisible
    {_tileTypeIsSolid :: Bool
    ,_tileTypeIsClimbable :: Bool
    }
  deriving (Eq,Show)
makeLenses ''TileType

-- load a new texture into a textured tiletype
loadTileTypeTextured :: Bool -> Bool -> FilePath -> Renderer -> IO TileType
loadTileTypeTextured isSolid isClimbable texPath renderer = loadTexture renderer texPath >>= return . (\t -> TileTypeTextured t isSolid isClimbable)

-- Load a BMP texture from a file
loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture renderer fp = do
  surface <- loadBMP fp
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  return texture

data Tile = Tile
  {_tileType      :: TileType
  ,_tileRectangle :: Rectangle CFloat
  }
  deriving (Eq,Show)
makeLenses ''Tile

-- Create a TileType at a position.
mkTile :: TileType -> Rectangle CFloat -> Tile
mkTile = Tile

-- A non-solid, white,1by1 tile at 0,0
defaultTile :: Tile
defaultTile = mkTile (TileTypeColored white False False) (Rectangle (P $ V2 0 0) (V2 1 1))

tilePos :: Lens' Tile Pos
tilePos = tileRectangle.rectPos

tilePosX :: Lens' Tile CFloat
tilePosX = tilePos.pos._x

tilePosY :: Lens' Tile CFloat
tilePosY = tilePos.pos._y

tileSize :: Lens' Tile Size
tileSize = tileRectangle.rectSize

tileWidth :: Lens' Tile CFloat
tileWidth = tileSize.size._x

tileHeight :: Lens' Tile CFloat
tileHeight = tileSize.size._y

tileL :: Lens' Tile CFloat
tileL = tilePosX

tileR :: Lens' Tile CFloat
tileR = lens (\t -> (t^.tileL) + (t^.tileWidth)) (\t r -> tileL .~ (r - (t^.tileWidth)) $ t)

tileT :: Lens' Tile CFloat
tileT = tilePosY

tileB :: Lens' Tile CFloat
tileB = lens (\t -> (t^.tileT) + (t^.tileHeight)) (\t b -> tileT .~ (b - (t^.tileHeight)) $ t)

tileTL :: Lens' Tile Pos 
tileTL = tilePos

tileTR :: Lens' Tile Pos 
tileTR = lens (\t -> (t^.tileTL) + (Pos $ V2 (t^.tileWidth) 0)) (\t tr -> tilePos .~ (tr - (Pos $ V2 (t^.tileWidth) 0)) $ t)

tileBL :: Lens' Tile Pos 
tileBL = lens (\t -> (t^.tileTL) + (Pos $ V2 0 (t^.tileHeight))) (\t bl -> tilePos .~ (bl - (Pos $ V2 0 (t^.tileHeight))) $ t)

tileBR :: Lens' Tile Pos 
tileBR = lens (\t -> (t^.tileTL) + (Pos $ V2 (t^.tileWidth) (t^.tileHeight))) (\t br -> tilePos .~ (br - (Pos $ V2 (t^.tileWidth) (t^.tileHeight))) $ t)

tileCenter :: Lens' Tile Pos 
tileCenter = lens (\t -> (t^.tileTL) + (Pos $ V2 (t^.tileWidth / 2) (t^.tileHeight / 2))) (\t c -> tilePos .~ (c - (Pos $ V2 (t^.tileWidth / 2) (t^.tileHeight / 2))) $ t)

-- Move a tile right and down by the given offset
moveTile :: V2 CFloat -> Tile -> Tile
moveTile o = tilePos+~Pos o

-- move a tile right
moveTileR :: CFloat -> Tile -> Tile
moveTileR r = tilePos+~(Pos $ V2 r 0)

-- move a tile left
moveTileL :: CFloat -> Tile -> Tile
moveTileL l = moveTileR (-1*l)

-- move a tile down
moveTileD :: CFloat -> Tile -> Tile
moveTileD d = tilePos+~(Pos $ V2 0 d)

-- move a tile up
moveTileU :: CFloat -> Tile -> Tile
moveTileU u = moveTileD (-1* u)

-- draw a tile
renderTile :: Renderer -> Tile -> IO ()
renderTile renderer t = case t^.tileType of
  TileTypeTextured tex _ _
    -> copy renderer tex Nothing $ Just $ floor <$> t^.tileRectangle

  TileTypeColored color _ _
    -> do rendererDrawColor renderer $= color
          fillRect renderer $ Just $ floor <$> t^.tileRectangle

  TileTypeInvisible _ _
    -> return ()

tileToHitBox :: Tile -> HitBox
tileToHitBox t = HitBoxRect (t^.tileRectangle)

