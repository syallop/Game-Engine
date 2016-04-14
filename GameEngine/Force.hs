{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}
module GameEngine.Force
  (Force(..)
  ,sumForce
  ,applyForce
  ,applyForces

  ,xComponent
  ,yComponent
  ,opposeX
  )
  where

import GameEngine.Velocity

import Control.Lens
import Data.Coerce
import Foreign.C.Types
import Linear

newtype Force = Force {_force :: V2 CFloat}
  deriving (Show,Eq,Num)

makeLenses ''Force

sumForce :: [Force] -> Force
sumForce = foldr (\f g -> Force $ (coerce f) + (coerce g)) (Force $ V2 0 0)

applyForce :: Force -> Velocity -> Velocity
applyForce (Force (V2 dX dY)) (Velocity (V2 x y)) = Velocity $ V2 (x + dX) (y + dY)

applyForces :: [Force] -> Velocity -> Velocity
applyForces fs = applyForce (sumForce fs)

xComponent :: Lens' Force CFloat 
xComponent = lens (view (force._x)) (flip (set (force._x)))

yComponent :: Lens' Force CFloat
yComponent = lens (view (force._y)) (flip (set (force._y)))

-- Create a force opposing a velocity in the X dimension with magnitude
opposeX :: CFloat -> Velocity -> Force
opposeX x v
  | movingLeft  v = Force $ V2 x 0
  | movingRight v = Force $ V2 (-1 * x) 0
  | otherwise     = Force $ V2 0 0

