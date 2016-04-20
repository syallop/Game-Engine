{-# LANGUAGE
    DeriveDataTypeable
  , ScopedTypeVariables
  , TemplateHaskell
  #-}
module GameEngine.Thing
  (Thing(..)
  ,thingTile
  ,thingIsSolid
  ,thingHasMass
  ,thingVelocity
  ,thingHealth
  ,thingHitBox
  ,thingContactDamage
  ,thingContactScore

  ,setMass
  ,setMassless

  ,moveThingRight,moveThingLeft,moveThingDown,moveThingUp
  ,moveThingRightBy,moveThingLeftBy,moveThingDownBy,moveThingUpBy
  ,moveThingBy

  ,tryMoveThingBy
  ,tryMoveThingByAcc

  ,collidesThing
  ,collidesThings
  ,filterCollidesThings

  ,touchesThing
  ,touchesThings
  ,filterTouchesThings

  ,contactDamage
  ,isDead

  ,solidHitBox
  ,presenceHitBox

  ,applyForceThing

  ,Things
  )
  where

import GameEngine.Counter
import GameEngine.Force
import GameEngine.HitBox
import GameEngine.Tile
import GameEngine.Velocity

import Control.Lens
import Data.Function
import Data.Map hiding (filter,map)
import Data.Text hiding (any,filter,map)
import Data.Typeable
import Foreign.C.Types
import Linear

-- A _thing_ with a drawable tile
data Thing = Thing
  {_thingTile          :: Tile     -- The tile tracks the position of the thing as the top left of its drawable rectangle
  ,_thingIsSolid       :: Bool     -- Whether the thing can be passed through/ pass through things
  ,_thingHasMass       :: Bool     -- Whether the thing is effected by gravity
  ,_thingVelocity      :: Velocity -- Velocity in x and y axis
  ,_thingHealth        :: Counter  -- Things own health has a min, a current and a max.
  ,_thingHitBox        :: HitBox   -- Area in which it counts as making contact with the thing

  ,_thingContactDamage :: CInt     -- Damage taken (/health gained) for making contact
  ,_thingContactScore  :: CInt     -- Points gained for making contact
  }
  deriving (Eq,Show,Typeable)
makeLenses ''Thing

type Things = Map Text Thing

setMass :: Thing -> Thing
setMass = set thingHasMass True

setMassless :: Thing -> Thing
setMassless = set thingHasMass False

-- Move a thing in a direction
moveThingRight, moveThingLeft, moveThingDown, moveThingUp :: Thing -> Thing
moveThingRight = moveThingRightBy 1
moveThingLeft  = moveThingLeftBy 1
moveThingDown  = moveThingDownBy 1
moveThingUp    = moveThingUpBy 1

-- move a thing in a direction by a positive amount
moveThingRightBy, moveThingLeftBy, moveThingDownBy, moveThingUpBy :: CFloat -> Thing -> Thing
moveThingRightBy x = over thingTile (moveTileR x)
moveThingLeftBy  x = over thingTile (moveTileL x)
moveThingDownBy  y = over thingTile (moveTileD y)
moveThingUpBy    y = over thingTile (moveTileU y)

-- move a thing in both axis
moveThingBy :: V2 CFloat -> Thing -> Thing
moveThingBy (V2 x y) thing = moveThingDownBy y . moveThingRightBy x $ thing


-- Try and move a thing in a direction. Left => validation function failed and velocity in that direction is nullified
tryMoveThingRight,tryMoveThingLeft,tryMoveThingDown,tryMoveThingUp :: Thing -> (Thing -> Bool) -> Either Thing Thing
tryMoveThingRight = tryMoveThing moveThingRight (over thingVelocity nullX)
tryMoveThingLeft  = tryMoveThing moveThingLeft  (over thingVelocity nullX)
tryMoveThingDown  = tryMoveThing moveThingDown  (over thingVelocity nullY)
tryMoveThingUp    = tryMoveThing moveThingUp    (over thingVelocity nullY)

-- Try and move a thing with a movement function. Applying a failure function if it fails a validation function.
-- Left => Validation function failed.
tryMoveThing :: (Thing -> Thing) -> (Thing -> Thing) -> Thing -> (Thing -> Bool) -> Either Thing Thing
tryMoveThing moveF failF thing isValid =
  let thing' = moveF thing
     in if isValid thing'
          then Right thing'
          else Left $ failF thing


-- Try and move a thing with a movement function. Applying a failure function if it fails an accumulating validation function.
-- Left => Validation failed. Return the accumulator.
tryMoveThingAcc :: (Thing -> Thing)
                -> (Thing -> Thing)
                -> (acc -> Thing -> (Bool,acc))
                -> Thing
                -> acc
                -> (Either Thing Thing,acc)
tryMoveThingAcc move fail validate thing acc =
  let thing' = move thing
     in case validate acc thing' of
          (valid,acc')
            | valid     -> (Right thing'       ,acc')
            | otherwise -> (Left . fail $ thing,acc')

tryMoveThingRightAcc,tryMoveThingLeftAcc,tryMoveThingDownAcc,tryMoveThingUpAcc :: acc -> (acc -> Thing -> (Bool,acc)) -> Thing -> (Either Thing Thing,acc)
tryMoveThingRightAcc acc validate thing = tryMoveThingAcc moveThingRight (over thingVelocity nullX) validate thing acc
tryMoveThingLeftAcc  acc validate thing = tryMoveThingAcc moveThingLeft  (over thingVelocity nullX) validate thing acc
tryMoveThingDownAcc  acc validate thing = tryMoveThingAcc moveThingDown  (over thingVelocity nullY) validate thing acc
tryMoveThingUpAcc    acc validate thing = tryMoveThingAcc moveThingUp    (over thingVelocity nullY) validate thing acc

-- Try and move a thing by a vector amount, stopping as soon as a validation function returns False.
tryMoveThingBy :: V2 CFloat -> Thing -> (Thing -> Bool) -> Thing
tryMoveThingBy (V2 x y) thing isValid = interleaveStateful (abs x) (abs y) thing fx fy
  where
    fx,fy :: CFloat -> Thing -> Either (Thing,CFloat) Thing
    fx = if x > 0 then fRight else fLeft
    fy = if y > 0 then fDown  else fUp

    fRight,fLeft,fDown,fUp :: CFloat -> Thing -> Either (Thing,CFloat) Thing
    fRight = step tryMoveThingRight
    fLeft  = step tryMoveThingLeft
    fDown  = step tryMoveThingDown
    fUp    = step tryMoveThingUp

    -- Apply a movement function to a thing, n times supporting early failure.
    -- E.G. if we hit a wall with 5 steps to go, theres no need to try another 5 times.
    step :: (Thing -> (Thing -> Bool) -> Either Thing Thing) -> CFloat -> Thing -> Either (Thing,CFloat) Thing
    step moveF delta thing
      | delta <= 0 = Right thing
      | otherwise  = case moveF thing isValid of
                         -- Failed to move => Done recursing
                         Left thing'
                           -> Right thing'

                         -- Moved. Recurse one less time
                         Right thing'
                           -> Left (thing',delta-1)



-- Try and move a thing by a vector amount, stopping as soon as a validation function returns False and accumulating a parameter
-- through each performed test.
tryMoveThingByAcc :: forall acc
                   . V2 CFloat
                  -> acc
                  -> Thing
                  -> (acc -> Thing -> (Bool,acc))
                  -> (Thing,acc)
tryMoveThingByAcc (V2 x y) acc thing validate = interleaveStateful (abs x) (abs y) (thing,acc) fx fy
  where
    {-fx,fy :: CFloat -> Thing -> Either (Thing,CFloat) Thing-}
    fx,fy :: CFloat -> (Thing,acc) -> Either ((Thing,acc), CFloat) (Thing,acc)
    fx = if x > 0 then fRight else fLeft
    fy = if y > 0 then fDown  else fUp

    fRight,fLeft,fDown,fUp :: CFloat -> (Thing,acc) -> Either ((Thing,acc), CFloat) (Thing,acc)
    fRight = step tryMoveThingRightAcc
    fLeft  = step tryMoveThingLeftAcc
    fDown  = step tryMoveThingDownAcc
    fUp    = step tryMoveThingUpAcc

    -- Apply a movement function to a thing, n times supporting early failure.
    -- E.G. if we hit a wall with 5 steps to go, theres no need to try another 5 times.
    step :: (acc -> (acc -> Thing -> (Bool,acc)) -> Thing -> (Either Thing Thing,acc))
         -> CFloat
         -> (Thing,acc)
         -> Either ((Thing,acc),CFloat) (Thing,acc)
    step moveF delta (thing,acc)
      | delta <= 0 = Right (thing,acc)
      | otherwise  = case moveF acc validate thing of

                         -- Failed to move => Done recursing
                         (Left thing',acc')
                           -> Right (thing',acc')

                         -- Moved. Recurse one less time
                         (Right thing',acc')
                           -> Left ((thing',acc'),delta-1)

-- TODO implement collide in terms of touch
-- Do two things collide?
collidesThing :: Thing -> Thing -> Bool
collidesThing = on collidesHitBox solidHitBox

-- Does a thing collide with a list of things?
collidesThings :: Thing -> [Thing] -> Bool
collidesThings = any . collidesThing

-- Filter Things which collide with a Thing
filterCollidesThings :: Thing -> [Thing] -> [Thing]
filterCollidesThings t = filter (collidesThing t)

-- Do two things touch (regardless of how solid they may be)?
touchesThing :: Thing -> Thing -> Bool
touchesThing = on collidesHitBox presenceHitBox

-- Does a thing touch with a list of things?
touchesThings :: Thing -> [Thing] -> Bool
touchesThings = any . touchesThing

-- Filter Things which touch a Thing
filterTouchesThings :: Thing -> [Thing] -> [Thing]
filterTouchesThings t = filter (touchesThing t)


-- No contact  => Nothing
-- Contact(/s) => Sum damage
contactDamage :: Thing -> [Thing] -> Maybe CInt
contactDamage t ts = case filterCollidesThings t ts of
  [] -> Nothing
  ts -> Just . sum . map _thingContactDamage $ ts

-- Has a thing died/ reached 0 Health?
isDead :: Thing -> Bool
isDead t = t^.thingHealth.to atMin



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

-- Calculate the effective HitBox of the solid area of a thing.
-- This takes into account the solidity and offset in the world, as tracked by the tile.
--
-- - A solid thing with a NoHitBox will use the tile boundaries as the effective HitBox.
-- - A non-solid thing will give no HitBox, regardless of whether one is set.
solidHitBox :: Thing -> HitBox
solidHitBox thing = case (thing^.thingHitBox,thing^.thingIsSolid) of
  -- We have no HitBox but are solid. Use the tile as a hitbox
  (NoHitBox,True)
    -> tileToHitBox (thing^.thingTile)

  -- We have no hitbox and we're non-solid
  (NoHitBox,False)
    -> NoHitBox

  -- We have a hit box but we're set non-solid
  (HitBoxRect _,False)
    -> NoHitBox

  -- We have a hit box and are solid, offset it
  (HitBoxRect r,True)
    -> HitBoxRect $ over rectPos (+ thing^.thingTile.tilePos) r

-- Calculate the effective HitBox of the area the thing is considered "present" in
-- (this is whether it is solid and able to be traditionally collided with or not)
-- Takes into account the offset in the world, as tracked by the tile.
--
-- - A solid thing with NoHitBox will use the tile boundaries as the effective HitBox.
-- - A non-solid thing will similarly still use its HitBox if present and its tile size otherwise.
presenceHitBox :: Thing -> HitBox
presenceHitBox thing = case thing^.thingHitBox of
  NoHitBox
    -> tileToHitBox (thing^.thingTile)

  HitBoxRect r
    -> HitBoxRect $ over rectPos (+ thing^.thingTile.tilePos) r

