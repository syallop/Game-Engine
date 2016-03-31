{-# LANGUAGE TemplateHaskell #-}
-- Defines individual square tiles in colored and textured varieties.
-- A tile has a position and a radius which can be queried and altered.
-- A tile can be rendered with 'renderTile'
module Game.Tile
  (TileColor
  ,redC,greenC,blueC,alphaC
  ,white,black,red,green,blue

  ,TileType(..)
  ,tileTypeColor
  ,tileTypeIsSolid
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

import Game.HitBox

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
alphaC = _x

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
    {_tileTypeColor   :: TileColor
    ,_tileTypeIsSolid :: Bool
    }

  | TileTypeTextured
    {_tileTypeTexture :: Texture
    ,_tileTypeIsSolid :: Bool
    }

  | TileTypeInvisible
    {_tileTypeIsSolid :: Bool
    }
  deriving (Eq,Show)
makeLenses ''TileType

-- load a new texture into a textured tiletype
loadTileTypeTextured :: Bool -> FilePath -> Renderer -> IO TileType
loadTileTypeTextured isSolid texPath renderer = loadTexture renderer texPath >>= return . (`TileTypeTextured` isSolid)

-- Load a BMP texture from a file
loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture renderer fp = do
  surface <- loadBMP fp
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  return texture

data Tile = Tile
  {_tileType      :: TileType
  ,_tileRectangle :: Rectangle CInt
  }
  deriving (Eq,Show)
makeLenses ''Tile

-- Create a TileType at a position.
mkTile :: TileType -> Rectangle CInt -> Tile
mkTile = Tile

-- A non-solid, white,1by1 tile at 0,0
defaultTile :: Tile
defaultTile = mkTile (TileTypeColored white False) (Rectangle (P $ V2 0 0) (V2 1 1))

tilePos :: Lens' Tile (Point V2 CInt)
tilePos = tileRectangle.rectPos

tilePosX :: Lens' Tile CInt
tilePosX = tilePos.lensP._x

tilePosY :: Lens' Tile CInt
tilePosY = tilePos.lensP._y

tileSize :: Lens' Tile (V2 CInt)
tileSize = tileRectangle.rectSize

tileWidth :: Lens' Tile CInt
tileWidth = tileSize._x

tileHeight :: Lens' Tile CInt
tileHeight = tileSize._y

tileL :: Lens' Tile CInt
tileL = tilePosX

tileR :: Lens' Tile CInt
tileR = lens (\t -> (t^.tileL) + (t^.tileWidth)) (\t r -> tileL .~ (r - (t^.tileWidth)) $ t)

tileT :: Lens' Tile CInt
tileT = tilePosY

tileB :: Lens' Tile CInt
tileB = lens (\t -> (t^.tileT) + (t^.tileHeight)) (\t b -> tileT .~ (b - (t^.tileHeight)) $ t)

tileTL :: Lens' Tile (Point V2 CInt)
tileTL = tilePos

tileTR :: Lens' Tile (Point V2 CInt)
tileTR = lens (\t -> (t^.tileTL) + (P $ V2 (t^.tileWidth) 0)) (\t tr -> tilePos .~ (tr - (P $ V2 (t^.tileWidth) 0)) $ t)

tileBL :: Lens' Tile (Point V2 CInt)
tileBL = lens (\t -> (t^.tileTL) + (P $ V2 0 (t^.tileHeight))) (\t bl -> tilePos .~ (bl - (P $ V2 0 (t^.tileHeight))) $ t)

tileBR :: Lens' Tile (Point V2 CInt)
tileBR = lens (\t -> (t^.tileTL) + (P $ V2 (t^.tileWidth) (t^.tileHeight))) (\t br -> tilePos .~ (br - (P $ V2 (t^.tileWidth) (t^.tileHeight))) $ t)

-- Move a tile right and down by the given offset
moveTile :: V2 CInt -> Tile -> Tile
moveTile o = tilePos+~P o

-- move a tile right
moveTileR :: CInt -> Tile -> Tile
moveTileR r = tilePos+~(P $ V2 r 0)

-- move a tile left
moveTileL :: CInt -> Tile -> Tile
moveTileL l = moveTileR (-1*l)

-- move a tile down
moveTileD :: CInt -> Tile -> Tile
moveTileD d = tilePos+~(P $ V2 0 d)

-- move a tile up
moveTileU :: CInt -> Tile -> Tile
moveTileU u = moveTileD (-1* u)

-- draw a tile
renderTile :: Renderer -> Tile -> IO ()
renderTile renderer t = case t^.tileType of
  TileTypeTextured tex _
    -> copy renderer tex Nothing $ Just $ t^.tileRectangle

  TileTypeColored color _
    -> do rendererDrawColor renderer $= color
          fillRect renderer $ Just $ t^.tileRectangle

  TileTypeInvisible _
    -> return ()

tileToHitBox :: Tile -> HitBox
tileToHitBox t = HitBoxRect (t^.tileRectangle)

