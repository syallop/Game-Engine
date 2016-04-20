{-# LANGUAGE
    ExistentialQuantification
  , OverloadedStrings
  , TemplateHaskell
  , StandaloneDeriving
  #-}
module GameEngine.Stage
  (Stage()
  ,setStage
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  ,stageBackground
  ,stageSubject
  ,stageCollectReproducing
  ,stageThings
  ,stageGravity
  ,stageSpeedLimit
  ,stageThingSpeedLimit
  ,stageScore

  ,tickStage

  ,applyForceSubject
  ,pushForceSubject

  -- Shouldnt be here, but for now, "Live" is a mess and probablt over abstracted so we're making
  -- choices about the types here.
  ,stageClient
  ,Subject
  ,StageReproducing
  ,StageLive
  ,StageClient
  ,StageAgent
  )
  where

import Control.Arrow
import Control.Lens
import Data.Map.Lens
import Data.Maybe
import Data.Text (Text)
import Foreign.C.Types
import Linear hiding (trace)
import Linear.Affine
import SDL
import qualified Data.Map as M

import GameEngine.AI
import GameEngine.Background
import GameEngine.Collect
import GameEngine.Counter
import GameEngine.Force
import GameEngine.HitBox
import GameEngine.Position
import GameEngine.Thing
import GameEngine.Tile
import GameEngine.TileGrid
import GameEngine.Velocity

import Debug.Trace

type Subject = Thing
type StageReproducing   = Reproducing Thing Subject ()
type StageLive        o = Live Thing Subject o
type StageClient        = Client Thing Thing Text ([StageReproducing],())
type StageAgent         = Agent (Subject,Thing) Text

data Stage = Stage
  {_stageBackground         :: Background
  ,_stageSubject            :: Subject
  ,_stageCollectReproducing :: Collect StageReproducing
  ,_stageGravity            :: Force

  ,_stageSpeedLimit         :: Velocity
  ,_stageThingSpeedLimit    :: Velocity

  ,_stageSubjectFriction    :: CFloat
  ,_stageThingFriction      :: CFloat

  ,_stageScore              :: CInt
  } deriving (Show,Eq)
makeLenses ''Stage


-- Update the stage by a single time step, given the number of ticks since the last update
tickStage :: CInt -> Stage -> Stage
tickStage dTicks
  = debugStage                   -- Execute stage debugging code
  . removeDeadThings             -- Remove any dead things
  . applyVelocityThings dTicks   -- Move things by their velocity
  . applySpeedLimitThings        -- Limit the velocity of things
  . applyFrictionThings          -- Reduce things velocity by friction if appropriate
  . updateThings                 -- Use things AI Agent to update them, potentially reproducing new things
  . applyGravityThings           -- Move things by the effect of gravity

  . applyVelocitySubject dTicks  -- Move subject by its velocity
  . applySpeedLimitSubject       -- Limit subjects velocity
  . applyFrictionSubject         -- Reduce subjects velocity by friction if appropriate
  . applyGravitySubject          -- Move subject by the effect of gravity

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background
         -> Subject
         -> Collect StageReproducing
         -> Force
         -> Velocity
         -> Velocity
         -> CFloat
         -> CFloat
         -> Maybe Stage
setStage b
         s
         things
         gravity
         subjectSpeedLimit
         thingSpeedLimit
         subjectFriction
         thingFriction     = Just $ Stage b s things gravity subjectSpeedLimit thingSpeedLimit subjectFriction thingFriction 0

-- Move a subject in a direction if they do not collide with the background
moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp :: Stage -> Maybe Stage
moveSubjectRight = moveSubjectRightBy 1
moveSubjectLeft  = moveSubjectLeftBy  1
moveSubjectDown  = moveSubjectDownBy  1
moveSubjectUp    = moveSubjectUpBy    1
-- Move a subject in a direction by a positive amount if they do not collide
-- with the background
moveSubjectRightBy, moveSubjectLeftBy, moveSubjectDownBy, moveSubjectUpBy :: CFloat -> Stage -> Maybe Stage
moveSubjectRightBy x = mapSubjectTile (moveTileR x)
moveSubjectLeftBy  x = mapSubjectTile (moveTileL x)
moveSubjectDownBy  y = mapSubjectTile (moveTileD y)
moveSubjectUpBy    y = mapSubjectTile (moveTileU y)

-- Map a function across the subjects tile IF the resulting tile does not
-- collide with the background
mapSubjectTile :: (Tile -> Tile) -> Stage -> Maybe Stage
mapSubjectTile f stg =
  let tile     = stg^.stageSubject.thingTile
      nextTile = f tile
     in setSubjectTile nextTile stg

-- Set the subject to the given tile, if it does not collide with
-- the background or any of the things
setSubjectTile :: Tile -> Stage -> Maybe Stage
setSubjectTile tile stg =
  let tileGrid = stg^.stageBackground.backgroundTileGrid
      subject  = stg^.stageSubject
      subject' = set thingTile tile subject
      things   = stageThings stg
     in if collidesTileGrid (solidHitBox subject') tileGrid
        || collidesThings subject' things
          then Nothing
          else Just $ set stageSubject subject' stg


-- Does a thing collide with anything on the stage (EXCEPT the subject)?
collidesAnything :: Stage -> Thing -> Bool
collidesAnything stg thing = collidesStageBackgroundTileGrid stg thing
                          || collidesStageThings             stg thing

-- Does a thing collide with the background tilegrid?
collidesStageBackgroundTileGrid :: Stage -> Thing -> Bool
collidesStageBackgroundTileGrid stg thing = collidesTileGrid (solidHitBox thing) (stg^.stageBackground.backgroundTileGrid)

-- Does a thing collide with any of the things on the stage?
collidesStageThings :: Stage -> Thing -> Bool
collidesStageThings stg thing = collidesThings thing (stageThings stg)

-- Extract a list of all the things on the stage, throwing away their names, controlling agents and anything else
stageThings :: Stage -> [Thing]
stageThings stg = map ((`withLiveClient` _client) . view reproducing . fst) . collected $ stg^.stageCollectReproducing

-- Which things does a thing collide with?
stageCollisions :: Thing -> Stage -> [Thing]
stageCollisions thing = filterCollidesThings thing . stageThings

-- Which things does a thing touch?
stageTouches :: Thing -> Stage -> [Thing]
stageTouches thing = filterTouchesThings thing . stageThings

-- Which things does a thing collide with? Cache the thing alongside the key.
stageCollisions' :: Thing -> Stage -> [(Key,Thing)]
stageCollisions' thing0 stg = foldCollect f [] (stg^.stageCollectReproducing)
  where f k mName r acc
          | collidesThing thing0 thing1 = (k,thing1):acc
          | otherwise                   = acc
          where thing1 = (`withLiveClient` _client) . view reproducing $ r

-- Which things does a thing touch? Cache the thing alongside the key.
stageTouches' :: Thing -> Stage -> [(Key,Thing)]
stageTouches' thing0 stg = foldCollect f [] (stg^.stageCollectReproducing)
  where f k mName r acc
          | touchesThing thing0 thing1 = (k,thing1):acc
          | otherwise                  = acc
          where thing1 = (`withLiveClient` _client) . view reproducing $ r


-- Apply velocity to the subject by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Checks collision with the background and other things.
-- - Apply collision damage
-- - Apply score increase
-- - Remove things which disappear on contact
applyVelocitySubject :: CInt -> Stage -> Stage
applyVelocitySubject ticks stg =
  let subject                             = stg^.stageSubject
      -- Modify the amount to move the subject proportional to the number of ticks
      baseDisplacement                    = subject^.thingVelocity.vel
      displacementModifier                = V2 (fromIntegral ticks / 10) (fromIntegral ticks / 10)
      displacement                        = baseDisplacement * displacementModifier

      -- Try and move the subject, accumulating a list of collisions and touches
      (movedSubject,(collisions,touches)) = tryMoveThingByAcc displacement ([],[]) subject validateSubjectMovement

      -- Total damage of collisions, score increase of touches
      collisionDamage                     = sum . map (_thingContactDamage . snd) $ collisions
      scoreIncrease                       = sum . map (_thingContactScore. snd)  $ touches
      -- Any touched things with "thingContactConsumed" set, should be removed.
      consumedKeys                        = map fst . filter (\(_,thing) -> thing^.thingContactConsumed) $ touches

      -- Updated score and subject accounting for damage
      newScore                            = stg^.stageScore + scoreIncrease
      damagedSubject                      = over thingHealth (subCounter collisionDamage) movedSubject
     in set stageScore newScore
      . set stageSubject damagedSubject
      . over stageCollectReproducing (\c -> foldr deleteKey c consumedKeys) -- remove all consumed things
      $ stg
  where
    -- Given an accumulated list of keys collided and keys just touching, test whether a thing comes to a stop.
    --
    -- Accumulated keys are also cached alongside the value of their thing when it was determined they collided/ touched.
    validateSubjectMovement :: ([(Key,Thing)],[(Key,Thing)]) -> Thing -> (Bool,([(Key,Thing)],[(Key,Thing)]))
    validateSubjectMovement (accCollisions,accTouches) testThing =
      let collidesTileGrid = collidesStageBackgroundTileGrid stg testThing
          thingCollisions  = stageCollisions' testThing stg
          collides         = collidesTileGrid || (not . null $ thingCollisions)
          thingTouches     = stageTouches' testThing stg
         in (not collides,(thingCollisions++accCollisions,thingTouches++accTouches))


-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: CInt -> Stage -> Stage
applyVelocityThings ticks stg = over (stageCollectReproducing . traverse) (over reproducing (mapLiveClient (over client applyVelocityThing))) stg
  where
    applyVelocityThing :: Thing -> Thing
    applyVelocityThing thing = tryMoveThingBy ((* V2 (fromIntegral ticks / 10) (fromIntegral ticks / 10)) $ thing^.thingVelocity.vel)
                                              thing
                                              (not . collidesStageBackgroundTileGrid stg . set thingIsSolid True) -- Force things to be considered solid against the background tiles

-- Apply acceleration due to gravity to the subject
applyGravitySubject :: Stage -> Stage
applyGravitySubject stg = applyForceSubject (stg^.stageGravity) stg

-- apply gravity to all of the Things
applyGravityThings :: Stage -> Stage
applyGravityThings stg = over (stageCollectReproducing . traverse) (over reproducing (mapLiveClient (over client (applyForceThing (stg^.stageGravity))))) stg

-- Apply a force to a subject to change its velocity
applyForceSubject :: Force -> Stage -> Stage
applyForceSubject force = over stageSubject (applyForceThing force)

-- Apply force to a subject, only if it is making contact with a solid object in the opposite
-- direction with which to 'push' off of.
pushForceSubject :: Force -> Stage -> Stage
pushForceSubject f stg
  | collidesStageBackgroundTileGrid stg (moveThingBy (V2 x y) $ stg^.stageSubject) = applyForceSubject f stg
  | otherwise = stg

  where
    x = if isPositive $ f^.xComponent then -1 else 1
    y = if isPositive $ f^.yComponent then -1 else 1

    isPositive = (>= 0)

-- reduce the subjects velocity if it has exceeded the limit
applySpeedLimitSubject :: Stage -> Stage
applySpeedLimitSubject stg = stageSubject.thingVelocity%~limitVelocity (stg^.stageSpeedLimit) $ stg

-- Reduce all things velocity if it has exceeded the limit
applySpeedLimitThings :: Stage -> Stage
applySpeedLimitThings stg = stageCollectReproducing -- Collect StageReproducing
                          . traverse                -- StageReproducing
                          . reproducing             -- Live Thing Subject ([Reproducing Thing Subject ()],())
                          %~ (mapLiveClient applySpeedLimitClient)
                           $ stg
  where applySpeedLimitClient :: Client Thing ob ac ([StageReproducing],()) -> Client Thing ob ac ([StageReproducing],())
        applySpeedLimitClient = client        -- Thing
                              . thingVelocity -- Velocity
                              %~ (limitVelocity (stg^.stageThingSpeedLimit))

-- Apply friction to the subject
applyFrictionSubject :: Stage -> Stage
applyFrictionSubject stg
  -- Standing on a tile
  | collidesStageBackgroundTileGrid stg (stg^.stageSubject.to (moveThingBy (V2 0 1)))
    = applyForceSubject (stg^.stageSubject.thingVelocity.to (opposeX (stg^.stageSubjectFriction))) stg

  -- Less air friction
  | otherwise
    = applyForceSubject (stg^.stageSubject.thingVelocity.to (opposeX 1)) stg

-- Apply friction to all things
applyFrictionThings :: Stage -> Stage
applyFrictionThings stg = stageCollectReproducing -- Collect (Reproducing Thing Pos ())
                        . traverse                -- Reproducing Thing Pos ()
                        . reproducing             -- Live Thing Pos ([Reproducing Thing Pos ()],())
                        %~ (mapLiveClient applyFrictionClient)
                         $ stg
  where
    applyFrictionClient = client%~applyFrictionThing (stg^.stageThingFriction)

    applyFrictionThing :: CFloat -> Thing -> Thing
    applyFrictionThing l t
      -- Standing on a tile
      | collidesStageBackgroundTileGrid stg (stg^.stageSubject.to (moveThingBy (V2 0 1)))
       = applyForceThing (t^.thingVelocity.to (opposeX l)) t

      -- Air friction
      | otherwise
       = applyForceThing (t^.thingVelocity.to (opposeX 1)) t

-- Update each thing by its corresponding agent
updateThings :: Stage -> Stage
updateThings stg
  = let thingInput = stg^.stageSubject
        (newThings,updatedThings) = mapWriteCollect (\k mName repThing0
                                                      -> let (repThing1,(newThings,_)) = updateReproducing thingInput repThing0
                                                            in (newThings,repThing1)
                                                    ) (stg^.stageCollectReproducing)
       in set stageCollectReproducing (fst $ insertAnonymouses newThings updatedThings) stg

-- Remove all dead things from the stage collection
removeDeadThings :: Stage -> Stage
removeDeadThings stg =
  let (deads,alives) = partitionCollect (\rep -> withLiveClient (rep^.reproducing) (isDead . _client)) (stg^.stageCollectReproducing)
     in set stageCollectReproducing alives stg


-- An example client. Handles Text actions, namely walkleft,walkright,jump,shootleft and shootright
stageClient :: Thing -> StageClient
stageClient t0 = mkClient t0 id applyActionThing
  where
    applyActionThing :: Text -> Thing -> (Thing,([StageReproducing],()))
    applyActionThing ac thing = case ac of
      "walkleft"
        -> (applyForceThing (Force $ V2 (-2) 0) thing,([],()))

      "walkright"
        -> (applyForceThing (Force $ V2 2 0) thing,([],()))

      "jump"
        -> (applyForceThing (Force $ V2 0 (-5)) thing,([],()))

      "shootleft"
        -> (thing,([bulletReproducing (-1) thing],()))

      "shootright"
        -> (thing,([bulletReproducing 1 thing],()))

      _
        -> (thing,([],()))

    bulletReproducing :: CFloat -> Thing -> StageReproducing
    bulletReproducing x thing = mkReproducing (bulletLive x thing)

    bulletLive :: CFloat -> Thing -> Live Thing Subject ([StageReproducing],())
    bulletLive x thing = mkLive (bulletClient x thing) bulletAgent

    bulletClient :: CFloat -> Thing -> Client Thing Thing Text ([StageReproducing],())
    bulletClient x thing = mkClient (bulletThing x thing) id (\ac t -> (t,([],())))

    bulletAgent :: Agent (Subject,Thing) Text
    bulletAgent = mkAgent () (\ob () -> ("",()))

    bulletThing :: CFloat -> Thing -> Thing
    bulletThing x thing = Thing
      {_thingTile            = bulletTile thing
      ,_thingIsSolid         = True
      ,_thingHasMass         = False
      ,_thingVelocity        = Velocity $ V2 x 0
      ,_thingHealth          = fromJust $ mkCounter 1 0 1
      ,_thingHitBox          = NoHitBox -- solid => entire tile is solid hitbox
      ,_thingContactDamage   = 1
      ,_thingContactScore    = 0
      ,_thingContactConsumed = True -- Consumed on hit with player
      }

    bulletTile :: Thing -> Tile
    bulletTile thing = mkTile (TileTypeColored (V4 1 1 1 1) True) (Rectangle (let Pos p = thing^.thingTile.tilePos in P p) (V2 10 10))

debugStage :: Stage -> Stage
debugStage stg = traceShow (stg^.stageScore) stg

