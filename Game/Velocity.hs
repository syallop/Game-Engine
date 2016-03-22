module Game.Velocity
  (Velocity(..)
  ,applyVelocity
  ,nullX
  ,nullY
  ,limitVelocity
  )
  where

import Linear
import Foreign.C.Types

newtype Velocity = Velocity {_vel :: V2 CInt}
  deriving (Show,Eq)

applyVelocity :: Velocity -> V2 CInt -> V2 CInt
applyVelocity (Velocity (V2 dX dY)) (V2 x y) = V2 (x + dX) (y + dY)

nullX :: Velocity -> Velocity
nullX (Velocity (V2 _ y)) = Velocity (V2 0 y)

nullY :: Velocity -> Velocity
nullY (Velocity (V2 x _)) = Velocity (V2 x 0)

-- Limit the magnitude of the velocity in either direction by a positive amount
-- TODO: Maybe limit absolute velocity. This method means you can travel faster in a diagonal
limitVelocity :: CInt -> Velocity -> Velocity
limitVelocity l (Velocity (V2 x y)) = Velocity $ V2 (limit l x) (limit l y)

limit :: CInt -> CInt -> CInt
limit l x
  | x < 0     = if x < (-1 * l) then (-1 * l) else x
  | otherwise = if x < l then x else l

