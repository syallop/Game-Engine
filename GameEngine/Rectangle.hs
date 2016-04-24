{-# LANGUAGE TemplateHaskell #-}
module GameEngine.Rectangle
  (Rectangle(..)
  ,rectPos
  ,rectSize
  ,rectBoundaries
  ,touchesRectangle
  ,outsideRectangle
  ,insideRectangle
  )
  where

import GameEngine.Position
import GameEngine.Size

import Control.Lens
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL

rectPos :: Lens' (Rectangle CFloat) Pos
rectPos = lens (\(Rectangle (P v) s) -> Pos v) (\(Rectangle (P p0) s) (Pos p1) -> Rectangle (P p1) s)

rectSize :: Lens' (Rectangle CFloat) Size 
rectSize = lens (\(Rectangle p s) -> Size s) (\(Rectangle p s0) (Size s1) -> Rectangle p s1)

-- Rectangle boundaries left,right,top,bottom
rectBoundaries :: Num a => Rectangle a -> V4 a
rectBoundaries (Rectangle (P (V2 lx ty)) (V2 w h)) = V4 lx (lx+w) ty (ty+h)

-- Do two rectangles touch?
touchesRectangle :: (Num a, Ord a) => Rectangle a -> Rectangle a -> Bool
touchesRectangle r0 r1 =
  let V4 left0 right0 top0 bottom0 = rectBoundaries r0
      V4 left1 right1 top1 bottom1 = rectBoundaries r1
     in and $ [left0   < right1
              ,right0  > left1
              ,top0    < bottom1
              ,bottom0 > top1
              ]

-- Is a rectangle fully outside another?
outsideRectangle :: (Num a,Ord a) => Rectangle a -> Rectangle a -> Bool
outsideRectangle r0 r1 =
  let V4 left0 right0 top0 bottom0 = rectBoundaries r0
      V4 left1 right1 top1 bottom1 = rectBoundaries r1
     in or [right0  < left1
           ,left0   > right1
           ,bottom0 < top1
           ,top0    > bottom1
           ]

-- Is a rectangle fully inside another?
insideRectangle :: (Num a, Ord a) => Rectangle a -> Rectangle a -> Bool
insideRectangle r0 = not . outsideRectangle r0

