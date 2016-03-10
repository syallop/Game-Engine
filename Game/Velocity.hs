module Game.Velocity
  (Velocity(..)
  ,applyVelocity
  )
  where

import Linear
import Foreign.C.Types

newtype Velocity = Velocity {_vel :: V2 CInt}
  deriving (Show,Eq)

applyVelocity :: Velocity -> V2 CInt -> V2 CInt
applyVelocity (Velocity (V2 dX dY)) (V2 x y) = V2 (x + dX) (y + dY)

