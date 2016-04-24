{-# LANGUAGE TemplateHaskell #-}
module GameEngine.HitBox
  (HitBox(..)
  ,hitBoxRectangle

  ,collidesHitBox
  ,hitBoxBoundaries

  ,rectPos
  ,rectSize
  )
  where

import GameEngine.Position
import GameEngine.Size
import GameEngine.Rectangle

import Control.Lens
import Data.Maybe
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL

-- A hit box
data HitBox
  = HitBoxRect
    {_hitBoxRectangle :: Rectangle CFloat
    }
  | NoHitBox
  deriving (Eq,Show)
makeLenses ''HitBox

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
hitBoxBoundaries :: HitBox -> Maybe (V4 CFloat)
hitBoxBoundaries NoHitBox       = Nothing
hitBoxBoundaries (HitBoxRect r) = Just $ rectBoundaries r 

