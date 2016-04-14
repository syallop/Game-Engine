{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}
module GameEngine.Velocity
  (Velocity(..)
  ,applyVelocity
  ,nullX
  ,nullY
  ,limitVelocity

  ,movingLeft
  ,movingRight
  ,movingDown
  ,movingUp

  ,vel
  )
  where

import GameEngine.Position

import Control.Lens
import Foreign.C.Types
import Linear

newtype Velocity = Velocity {_vel :: V2 CFloat}
  deriving (Show,Eq,Num)

makeLenses ''Velocity

applyVelocity :: Velocity -> Pos -> Pos 
applyVelocity (Velocity (V2 dX dY)) (Pos (V2 x y)) = Pos $ V2 (x + dX) (y + dY)

nullX :: Velocity -> Velocity
nullX (Velocity (V2 _ y)) = Velocity (V2 0 y)

nullY :: Velocity -> Velocity
nullY (Velocity (V2 x _)) = Velocity (V2 x 0)

-- Limit the magnitude of the velocity in either direction by a positive amount
-- TODO: Maybe limit absolute velocity. This method means you can travel faster in a diagonal
limitVelocity :: Velocity -> Velocity -> Velocity
limitVelocity (Velocity (V2 lx ly)) (Velocity (V2 vx vy)) = Velocity $ V2 (limit lx vx) (limit ly vy)

limit :: CFloat -> CFloat -> CFloat 
limit l x
  | x < 0     = if x < (-1 * l) then (-1 * l) else x
  | otherwise = if x < l then x else l

movingLeft,movingRight,movingDown,movingUp :: Velocity -> Bool
movingLeft  (Velocity (V2 x y)) = x < 0
movingRight (Velocity (V2 x y)) = 0 < x
movingDown  (Velocity (V2 x y)) = 0 < y
movingUp    (Velocity (V2 x y)) = y < 0

