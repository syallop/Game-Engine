{-# LANGUAGE TemplateHaskell #-}
module Game.Thing
  (Thing(..)
  ,thingTile
  ,thingIsSolid
  ,thingHasMass
  ,thingVelocity
  ,thingHealth

  ,setMass
  ,setMassless

  ,moveThingRight,moveThingLeft,moveThingDown,moveThingUp
  ,moveThingRightBy,moveThingLeftBy,moveThingDownBy,moveThingUpBy
  ,moveThingBy

  ,tryMoveThingBy

  ,collidesThing
  ,collidesThings

  ,applyForceThing

  ,Things
  )
  where

import Foreign.C.Types

import Linear
import Game.Tile
import Game.Velocity
import Game.Force
import Game.Counter

import Data.Map
import Data.Text hiding (any)

import Control.Lens

-- A _thing_ with a drawable tile
data Thing = Thing
  {_thingTile     :: Tile
  ,_thingIsSolid  :: Bool
  ,_thingHasMass  :: Bool
  ,_thingVelocity :: Velocity
  ,_thingHealth   :: Counter
  }
  deriving (Eq,Show)

makeLenses ''Thing

type Things = Map Text Thing

setMass :: Thing -> Thing
setMass t = t{_thingHasMass = True}

setMassless :: Thing -> Thing
setMassless t = t{_thingHasMass = False}

-- Move a thing in a direction
moveThingRight, moveThingLeft, moveThingDown, moveThingUp :: Thing -> Thing
moveThingRight = moveThingRightBy 1
moveThingLeft  = moveThingLeftBy 1
moveThingDown  = moveThingDownBy 1
moveThingUp    = moveThingUpBy 1

-- move a thing in a direction by a positive amount
moveThingRightBy, moveThingLeftBy, moveThingDownBy, moveThingUpBy :: CInt -> Thing -> Thing
moveThingRightBy x = over thingTile (moveTileR x)
moveThingLeftBy  x = over thingTile (moveTileL x)
moveThingDownBy  y = over thingTile (moveTileD y)
moveThingUpBy    y = over thingTile (moveTileU y)

-- move a thing in both axis
moveThingBy :: V2 CInt -> Thing -> Thing
moveThingBy (V2 x y) thing = moveThingDownBy y . moveThingRightBy x $ thing


-- Try and move a thing in a direction. Left => validation function failed and velocity in that direction is nullified
tryMoveThingRight,tryMoveThingLeft,tryMoveThingDown,tryMoveThingUp :: Thing -> (Tile -> Bool) -> Either Thing Thing
tryMoveThingRight thing isValid =
  let thing' = moveThingRight thing
     in if isValid $ thing'^.thingTile
          then Right thing'
          else Left $ over thingVelocity nullX thing

tryMoveThingLeft thing isValid =
  let thing' = moveThingLeft thing
     in if isValid $ thing'^.thingTile
          then Right thing'
          else Left $ over thingVelocity nullX thing

tryMoveThingDown thing isValid =
  let thing' = moveThingDown thing
     in if isValid $ thing'^.thingTile
          then Right thing'
          else Left $ over thingVelocity nullY thing

tryMoveThingUp thing isValid =
  let thing' = moveThingUp thing
     in if isValid $ thing'^.thingTile
          then Right thing'
          else Left $ over thingVelocity nullY thing


tryMoveThingBy :: V2 CInt -> Thing -> (Tile -> Bool) -> Thing
tryMoveThingBy (V2 x y) thing isValid = interleaveStateful (abs x) (abs y) thing fx fy
  where
    fx :: CInt -> Thing -> Either (Thing,CInt) Thing
    fx = if x > 0 then fRight else fLeft
    fy = if y > 0 then fDown  else fUp

    fRight = step tryMoveThingRight
    fLeft  = step tryMoveThingLeft
    fDown  = step tryMoveThingDown
    fUp    = step tryMoveThingUp

    -- Apply a movement function to a thing, n times supporting early failure.
    -- E.G. if we hit a wall with 5 steps to go, theres no need to try another 5 times.
    step :: (Thing -> (Tile -> Bool) -> Either Thing Thing) -> CInt -> Thing -> Either (Thing,CInt) Thing
    step _ 0 thing         = Right thing
    step moveF delta thing = case moveF thing isValid of
                                 -- Failed to move => Done recursing
                                 Left thing'
                                   -> Right thing'

                                 -- Moved. Recurse one less time
                                 Right thing'
                                   -> Left (thing',delta-1)


-- Does a tile collide with a Thing?
collidesThing :: Tile -> Thing -> Bool
collidesThing t0 thing = let t1 = thing^.thingTile in
  and [thing^.thingIsSolid
      ,t0^.tileL < t1^.tileR
      ,t0^.tileR > t1^.tileL
      ,t0^.tileT < t1^.tileB
      ,t0^.tileB > t1^.tileT
      ]

collidesThings :: Tile -> [Thing] -> Bool
collidesThings t0 = any (collidesThing t0)


{- Utils -}

-- Alternate functions left to right until one Hits a Right, then iterate the remaining function until
-- it too hits Righ. Return the accumulated state.
interleaveStateful :: a -> b -> s -> (a -> s -> Either (s,a) s) -> (b -> s -> Either (s,b) s) -> s
interleaveStateful = interleaveStatefulL

-- Apply the left function. Interleave to right or if done, iterate the right
interleaveStatefulL :: a -> b -> s -> (a -> s -> Either (s,a) s) -> (b -> s -> Either (s,b) s) -> s
interleaveStatefulL a b st fa fb = case fa a st of
  Left (st',a')
    -> interleaveStatefulR a' b st' fa fb

  Right st'
    -> iterateStateful b st' fb

-- Apply the right function. Interleave to left or if done, iterate the left
interleaveStatefulR :: a -> b -> s -> (a -> s -> Either (s,a) s) -> (b -> s -> Either (s,b) s) -> s
interleaveStatefulR a b st fa fb = case fb b st of
  Left (st',b')
    -> interleaveStatefulL a b' st' fa fb

  Right st'
    -> iterateStateful a st' fa

-- Iterate a function until Right
iterateStateful :: a -> s -> (a -> s -> Either (s,a) s) -> s
iterateStateful a st fa = case fa a st of
  Left (st',a')
    -> iterateStateful a' st' fa

  Right st'
    -> st'

-- Apply a force to a thing, changing its velocity if it has mass.
applyForceThing :: Force -> Thing -> Thing
applyForceThing (Force (V2 aX aY)) thing =
  if thing^.thingHasMass
    then over thingVelocity (\(Velocity (V2 vX vY)) -> Velocity $ V2 (vX + aX) (vY + aY)) thing
    else thing

