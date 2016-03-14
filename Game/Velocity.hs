module Game.Velocity
  (Velocity(..)
  ,applyVelocity
  ,nullX
  ,nullY
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

