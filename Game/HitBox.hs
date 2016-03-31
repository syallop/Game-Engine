{-# LANGUAGE TemplateHaskell #-}
module Game.HitBox
  (HitBox(..)
  ,hitBoxRectangle

  ,collidesHitBox
  ,hitBoxBoundaries

  ,rectPos
  ,rectSize
  )
  where

import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL
import Control.Lens

-- A hit box
data HitBox
  = HitBoxRect
    {_hitBoxRectangle :: Rectangle CInt
    }
  | NoHitBox
  deriving (Eq,Show)
makeLenses ''HitBox

rectPos :: Lens' (Rectangle a) (Point V2 a)
rectPos = lens (\(Rectangle p s) -> p) (\(Rectangle p0 s) p1 -> Rectangle p1 s)

rectSize :: Lens' (Rectangle a) (V2 a)
rectSize = lens (\(Rectangle p s) -> s) (\(Rectangle p s0) s1 -> Rectangle p s1)

collidesHitBox :: HitBox -> HitBox -> Bool
collidesHitBox h0 h1 = fromMaybe False $ do
  V4 left0 right0 top0 bottom0 <- hitBoxBoundaries h0
  V4 left1 right1 top1 bottom1 <- hitBoxBoundaries h1
  return $ and [left0   < right1
               ,right0  > left1
               ,top0    < bottom1
               ,bottom0 > top1
               ]

-- leftX,rightX,topY and bottomY positions
hitBoxBoundaries :: HitBox -> Maybe (V4 CInt)
hitBoxBoundaries NoHitBox       = Nothing
hitBoxBoundaries (HitBoxRect r) = Just $ V4 left right top bottom
  where
    left   = r^.rectPos._x
    right  = left + r^.rectSize._x
    top    = r^.rectPos._y
    bottom = top + r^.rectSize._y

