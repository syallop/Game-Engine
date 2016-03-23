{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game.Force
  (Force(..)
  ,sumForce
  ,applyForce
  ,applyForces

  ,xComponent
  ,yComponent
  ,opposeX
  )
  where

import Linear
import Foreign.C.Types
import Data.Coerce

import Game.Velocity

newtype Force = Force {_force :: V2 CInt}
  deriving (Show,Eq,Num)

sumForce :: [Force] -> Force
sumForce = foldr (\f g -> Force $ (coerce f) + (coerce g)) (Force $ V2 0 0)

applyForce :: Force -> Velocity -> Velocity
applyForce (Force (V2 dX dY)) (Velocity (V2 x y)) = Velocity $ V2 (x + dX) (y + dY)

applyForces :: [Force] -> Velocity -> Velocity
applyForces fs = applyForce (sumForce fs)

xComponent :: Force -> CInt
xComponent (Force (V2 x _)) = x

yComponent :: Force -> CInt
yComponent (Force (V2 _ y)) = y

-- Create a force opposing a velocity in the X dimension with magnitude
opposeX :: CInt -> Velocity -> Force
opposeX x v
  | movingLeft  v = Force $ V2 x 0
  | movingRight v = Force $ V2 (-1 * x) 0
  | otherwise     = Force $ V2 0 0

