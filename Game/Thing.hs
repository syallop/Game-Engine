module Game.Thing
  (Thing(..)
  ,moveThingRight,moveThingLeft,moveThingDown,moveThingUp
  ,moveThingRightBy,moveThingLeftBy,moveThingDownBy,moveThingUpBy
  ,mapThingTile
  ,thingTile
  )
  where

import Foreign.C.Types

import Game.Tile

-- A _thing_ with a drawable tile
data Thing = Thing
  {_thingTile :: Tile
  }
  deriving Show

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

-- map a function across a things tile
mapThingTile :: (Tile -> Tile) -> Thing -> Thing
mapThingTile f thing = Thing{_thingTile = f . _thingTile $ thing}

thingTile :: Thing -> Tile
thingTile = _thingTile

