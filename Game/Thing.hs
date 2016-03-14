module Game.Thing
  (Thing(..)
  ,moveThingRight,moveThingLeft,moveThingDown,moveThingUp
  ,moveThingRightBy,moveThingLeftBy,moveThingDownBy,moveThingUpBy
  ,moveThingBy
  ,mapThingTile
  ,thingTile

  ,collidesThing
  ,collidesThings

  ,mapVelocity
  )
  where

import Foreign.C.Types

import Linear
import Game.Tile
import Game.Velocity

-- A _thing_ with a drawable tile
data Thing = Thing
  {_thingTile :: Tile
  ,_isSolid   :: Bool
  ,_velocity  :: Velocity
  }
  deriving (Eq,Show)

-- Move a thing in a direction
moveThingRight, moveThingLeft, moveThingDown, moveThingUp :: Thing -> Thing
moveThingRight = moveThingRightBy 1
moveThingLeft  = moveThingLeftBy 1
moveThingDown  = moveThingDownBy 1
moveThingUp    = moveThingUpBy 1

-- move a thing in a direction by a positive amount
moveThingRightBy, moveThingLeftBy, moveThingDownBy, moveThingUpBy :: CInt -> Thing -> Thing
moveThingRightBy x = mapThingTile (moveR x)
moveThingLeftBy  x = mapThingTile (moveL x)
moveThingDownBy  y = mapThingTile (moveD y)
moveThingUpBy    y = mapThingTile (moveU y)

moveThingBy :: V2 CInt -> Thing -> Thing
moveThingBy (V2 x y) thing = moveThingDownBy y . moveThingRightBy x $ thing

-- map a function across a things tile
mapThingTile :: (Tile -> Tile) -> Thing -> Thing
mapThingTile f thing = thing{_thingTile = f . _thingTile $ thing}

thingTile :: Thing -> Tile
thingTile = _thingTile

-- Does a tile collide with a Thing?
collidesThing :: Tile -> Thing -> Bool
collidesThing t0 thing = let t1 = _thingTile thing in
  and [_isSolid thing
      ,posX t0               < (posX t1 + radius t1)
      ,(posX t0 + radius t0) > posX t1
      ,posY t0               < (posY t1 + radius t1)
      ,(posY t0 + radius t0) > posY t1
      ]

collidesThings :: Tile -> [Thing] -> Bool
collidesThings t0 = any (collidesThing t0)

mapVelocity :: (Velocity -> Velocity) -> Thing -> Thing
mapVelocity f thing = thing{_velocity = f . _velocity $ thing}

