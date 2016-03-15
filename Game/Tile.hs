-- Defines individual square tiles in colored and textured varieties.
-- A tile has a position and a radius which can be queried and altered.
-- A tile can be rendered with 'renderTile'
module Game.Tile
  (TileColor
  ,white,black,red,green,blue

  ,Tile()
  ,defaultTile
  ,colorTile
  ,textureTile
  ,loadTextureTile
  ,invisibleTile

  ,posX
  ,posY
  ,radius

  ,leftX
  ,rightX
  ,topY
  ,bottomY

  ,move
  ,moveR
  ,moveL
  ,moveD
  ,moveU

  ,setColor
  ,setRadius
  ,mapPos

  ,renderTile

  ,loadTexture
  )
  where

import SDL
import Linear
import Linear.Affine
import Foreign.C.Types
import GHC.Word

-- Alias for a vector of RGBA
type TileColor = V4 Word8

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

-- x coordinate of left of tile
posX :: Tile -> CInt
posX   t = let Rectangle (P (V2 x _)) _        = _tileRectangle t in x

-- y coordinate of top of tile
posY :: Tile -> CInt
posY   t = let Rectangle (P (V2 _ y)) _        = _tileRectangle t in y

leftX :: Tile -> CInt
leftX = posX

rightX :: Tile -> CInt
rightX t = leftX t + radius t

topY :: Tile -> CInt
topY = posY

bottomY :: Tile -> CInt
bottomY t = topY t + radius t

-- radius of tile
radius :: Tile -> CInt
radius t = let Rectangle _            (V2 r _) = _tileRectangle t in r

-- Move a tile right and down by the given offset
move :: V2 CInt -> Tile -> Tile
move (V2 dx dy) = moveD dy . moveR dx

-- move a tile right
moveR :: CInt -> Tile -> Tile
moveR d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 (x+d) y)) r
             }

-- move a tile left
moveL :: CInt -> Tile -> Tile
moveL d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 (x-d) y)) r
             }

-- move a tile down
moveD :: CInt -> Tile -> Tile
moveD d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 x (y+d))) r
             }

-- move a tile up
moveU :: CInt -> Tile -> Tile
moveU d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 x (y-d))) r
             }


-- set the color of a tile
setColor :: TileColor -> Tile -> Tile
setColor c t = t{_tileColor = c}

-- set the radius of a tile
setRadius :: CInt -> Tile -> Tile
setRadius r t = t{_tileRectangle
                      = let Rectangle p _ = _tileRectangle t
                           in Rectangle p (V2 r r)
                 }

mapPos :: (V2 CInt -> V2 CInt) -> Tile -> Tile
mapPos f t = t{_tileRectangle = let Rectangle (P p) r = _tileRectangle t
                                   in Rectangle (P $ f p) r
              }

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

-- create an invisible tile with a position and radius
invisibleTile :: Point V2 CInt -> CInt -> Tile
invisibleTile pos radius = InvisibleTile (Rectangle pos (V2 radius radius))

