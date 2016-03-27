{-# LANGUAGE TemplateHaskell #-}
-- Defines individual square tiles in colored and textured varieties.
-- A tile has a position and a radius which can be queried and altered.
-- A tile can be rendered with 'renderTile'
module Game.Tile
  (TileColor
  ,redC,greenC,blueC,alphaC
  ,white,black,red,green,blue

  ,Tile()
  ,defaultTile
  ,colorTile
  ,textureTile
  ,loadTextureTile
  ,invisibleTile

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

  ,tileColor
  ,tileTexture

  ,moveTile
  ,moveTileR
  ,moveTileL
  ,moveTileD
  ,moveTileU

  ,renderTile
  ,loadTexture
  )
  where

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

-- A square with either a color or a texture
data Tile

  -- A rectangle with a color
  = ColorTile
    {_tileRectangle :: Rectangle CInt
    ,_tileColor     :: TileColor
    }

  -- A rectangle with a texture
  | TextureTile
    {_tileRectangle :: Rectangle CInt
    ,_tileTexture   :: Texture
    }

  | InvisibleTile
    {_tileRectangle :: Rectangle CInt
    }
  deriving Eq

instance Show Tile where
  show t = case t of
    ColorTile r c   -> "ColorTile " ++ show r ++ " " ++ show c
    TextureTile r t -> "TextureTile " ++ show r
    InvisibleTile r -> "InvisibleTile " ++ show r

makeLenses ''Tile

rectPos :: Lens' (Rectangle a) (Point V2 a)
rectPos = lens (\(Rectangle p s) -> p) (\(Rectangle p0 s) p1 -> Rectangle p1 s)

rectSize :: Lens' (Rectangle a) (V2 a)
rectSize = lens (\(Rectangle p s) -> s) (\(Rectangle p s0) s1 -> Rectangle p s1)


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


-- the default tile is at (0,0) has a radius of 1 and is white
defaultTile :: Tile
defaultTile = ColorTile (Rectangle (P $ V2 0 0) (V2 1 1)) white

-- draw a tile
renderTile :: Renderer -> Tile -> IO ()
renderTile renderer t = case t of
  ColorTile r c
    -> do rendererDrawColor renderer $= _tileColor t
          fillRect renderer $ Just r

  TextureTile r t
    -> copy renderer t Nothing $ Just r

  InvisibleTile r
    -> return ()
    -- -> drawRect renderer $ Just r

-- create a ColorTile with a color posiiton and radius
colorTile :: TileColor -> Point V2 CInt -> CInt -> Tile
colorTile color pos radius = ColorTile (Rectangle pos (V2 radius radius)) color

-- create an invisible tile with a position and radius
invisibleTile :: Point V2 CInt -> CInt -> Tile
invisibleTile pos radius = InvisibleTile (Rectangle pos (V2 radius radius))

-- load a bmp texture from a file and create a texture tile with a position and radius
loadTextureTile :: Renderer -> FilePath -> Point V2 CInt -> CInt -> IO Tile
loadTextureTile renderer fp pos radius = loadTexture renderer fp >>= \tex -> return $ textureTile tex pos radius

-- create a TextureTile with a texture, position and radius
textureTile :: Texture -> Point V2 CInt -> CInt -> Tile
textureTile texture pos radius = TextureTile (Rectangle pos (V2 radius radius)) texture

-- Load a BMP texture from a file
loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture renderer fp = do
  surface <- loadBMP fp
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  return texture

