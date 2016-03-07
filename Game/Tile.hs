module Game.Tile
  (TileColor
  ,white,black,red,green,blue

  ,Tile()
  ,defaultTile
  ,tile

  ,posX
  ,posY
  ,radius
  ,moveR
  ,moveL
  ,moveD
  ,moveU
  ,setColor
  ,setRadius

  ,drawTile
  )
  where

import SDL
import Linear
import Linear.Affine
import Foreign.C.Types
import GHC.Word

type TileColor = V4 Word8

white,black,red,green,blue :: TileColor

white = V4 maxBound maxBound maxBound maxBound
black = V4 minBound minBound minBound maxBound
red   = V4 maxBound minBound minBound maxBound
green = V4 minBound maxBound minBound maxBound
blue  = V4 minBound minBound maxBound maxBound


data Tile = Tile
  {_tileRectangle :: Rectangle CInt
  ,_tileColor     :: TileColor
  }

posX (Tile (Rectangle (P (V2 x _)) _) _) = x
posY (Tile (Rectangle (P (V2 _ y)) _) _) = y
radius (Tile (Rectangle _ (V2 r _)) _) = r

moveR d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 (x+d) y)) r
             }
moveL d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 (x-d) y)) r
             }

moveD d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 x (y+d))) r
             }
moveU d t = t{_tileRectangle
                  = let Rectangle (P (V2 x y)) r = _tileRectangle t
                       in Rectangle (P (V2 x (y-d))) r
             }


setColor c t = t{_tileColor = c}
setRadius r t = t{_tileRectangle
                      = let Rectangle p _ = _tileRectangle t
                           in Rectangle p (V2 r r)
                 }

defaultTile :: Tile
defaultTile = Tile (Rectangle (P $ V2 0 0) (V2 20 20)) (V4 maxBound maxBound maxBound maxBound)

drawTile :: Renderer -> Tile -> IO ()
drawTile renderer t = do
  rendererDrawColor renderer $= _tileColor t
  fillRect renderer $ Just $ _tileRectangle t

tile :: TileColor -> Point V2 CInt -> CInt -> Tile
tile color pos radius = Tile (Rectangle pos (V2 radius radius)) color

