{-# LANGUAGE
    ExistentialQuantification
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
  , StandaloneDeriving
  #-}
module GameEngine.Stage
  (Stage()
  ,setStage

  -- Main function to drive the updating of the stage
  ,tickStage

  -- Perform safe operations on the stage
  -- Interleaving these between "tickStage" should work as expected
  ,applyForceSubject
  ,pushForceSubject
  ,addUs
  ,remainingConsumable

  -- Unsafe operations
  -- These functions update the subject in various ways which IGNORE the rest of the stage.
  -- E.G. will allow phasing through solid objects. Only use if necessary!
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  -- Lenses into the stage state
  ,stageBackground
  ,stageSubject
  ,stageUs
  ,stageThem
  ,stageThings
  ,stageGravity
  ,stageSpeedLimit
  ,stageThingSpeedLimit
  ,stageScore

  -- Shouldnt be here, but for now, "Live" is a mess and probablt over abstracted so we're making
  -- choices about the types here.
  ,stageClient
  ,Subject
  ,StageReproducing
  ,StageLive
  ,StageClient
  ,StageAgent
  ,bullet
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
  {_stageBackground      :: Background
  ,_stageSubject         :: Subject

  ,_stageUs              :: Collect StageReproducing
  ,_stageThem            :: Collect StageReproducing

  ,_stageGravity         :: Force

  ,_stageSpeedLimit      :: Velocity
  ,_stageThingSpeedLimit :: Velocity

  ,_stageSubjectFriction :: CFloat
  ,_stageThingFriction   :: CFloat

  ,_stageScore           :: CInt
  } deriving (Show,Eq)
makeLenses ''Stage


-- Update the stage by a single time step, given the number of ticks since the last update
tickStage :: CInt -> Stage -> Stage
tickStage dTicks
  = debugStage                   -- Execute stage debugging code
  . removeDeadThings             -- Remove any dead things

  . applyVelocityThem dTicks     -- Move "them" things by their velocity
  . applySpeedLimitThem          -- Limit the velocity of "them" things
  . applyFrictionThem            -- Reduce things velocity by friction if appropriate
  . updateThem                   -- Use "them" things AI Agent to update them, potentially reproducing new things
  . applyGravityThem             -- Move "them" things by the effect of gravity

  . applyVelocityUs dTicks       -- Move "us" things by their velocity
  . applySpeedLimitUs            -- Limit the velocity of "us" things
  . applyFrictionUs              -- Reduce "us" things velocity by friction if appropriate
  . updateUs                     -- Use "us" things AI Agent to update them, potentially reproducing new things
  . applyGravityUs               -- Move "us" things by the effect of gravity

  . applyVelocitySubject dTicks  -- Move subject by its velocity
  . applySpeedLimitSubject       -- Limit subjects velocity
  . applyFrictionSubject         -- Reduce subjects velocity by friction if appropriate
  . applyGravitySubject          -- Move subject by the effect of gravity

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background               -- Image to use as background
         -> Subject                  -- Player Thing
         -> Collect StageReproducing -- Things considered to be "us" (our bullets, etc), not checked for collisions against subject
         -> Collect StageReproducing -- Things considered to be "them" (enemies,their bullets, etc), not checked for collisions against themselves
         -> Force                    -- Force of gravity
         -> Velocity                 -- Speed limit for the subject
         -> Velocity                 -- Speed limit for other things
         -> CFloat                   -- Friction for the subject
         -> CFloat                   -- Friction for other things
         -> Maybe Stage
setStage b
         s
         us
         them
         gravity
         subjectSpeedLimit
         thingSpeedLimit
         subjectFriction
         thingFriction     = Just $ Stage b s us them gravity subjectSpeedLimit thingSpeedLimit subjectFriction thingFriction 0

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

-- Does a thing touch the background tilegrid?
touchesStageBackgroundTileGrid :: Stage -> Thing -> Bool
touchesStageBackgroundTileGrid stg thing = collidesTileGrid (presenceHitBox thing) (stg^.stageBackground.backgroundTileGrid)

-- Does a thing collide with any of the things on the stage?
collidesStageThings :: Stage -> Thing -> Bool
collidesStageThings stg thing = collidesThings thing (stageThings stg)

-- Extract a list of all the things on the stage, throwing away their names, controlling agents and anything else
stageThings :: Stage -> [Thing]
stageThings stg = map ((`withLiveClient` _client) . view reproducing . fst) . collected $ stg^.stageThem


-- Which "them" things does an "us" thing collide with? Cache the thing alongside the key.
stageCollisionsThem :: Thing -> Stage -> [(Key,Thing)]
stageCollisionsThem = stageCollisions stageThem

-- Which "us" things does a "them" thing collide with? Cache the thing alongside the key.
stageCollisionsUs :: Thing -> Stage -> [(Key,Thing)]
stageCollisionsUs = stageCollisions stageUs

-- Which things does a thing collide with? Cache the thing alongside the key.
stageCollisions :: Lens' Stage (Collect StageReproducing) -> Thing -> Stage -> [(Key,Thing)]
stageCollisions thingsL thing0 stg = foldCollect f [] (stg^.thingsL)
  where f k mName r acc
          | collidesThing thing0 thing1 = (k,thing1):acc
          | otherwise                   = acc
          where thing1 = (`withLiveClient` _client) . view reproducing $ r

-- Which "them" things does an "us" thing touch? Cache the thing alongside the key.
stageTouchesThem :: Thing -> Stage -> [(Key,Thing)]
stageTouchesThem = stageTouches stageThem

-- Which "us" things does a "them" thing touch? Cache the thing alongside the key.
stageTouchesUs :: Thing -> Stage -> [(Key,Thing)]
stageTouchesUs = stageTouches stageUs

-- Which things does a thing touch? Cache the thing alongside the key.
stageTouches :: Lens' Stage (Collect StageReproducing) -> Thing -> Stage -> [(Key,Thing)]
stageTouches thingsL thing0 stg = foldCollect f [] (stg^.thingsL)
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
  let (stg',subject') = applyVelocityThing ticks stageThem (stg^.stageSubject) stg
     in set stageSubject subject' stg'

applyVelocityThem :: CInt -> Stage -> Stage
applyVelocityThem = applyVelocityThings stageThem stageUs

applyVelocityUs :: CInt -> Stage -> Stage
applyVelocityUs = applyVelocityThings stageUs stageThem


-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: Lens' Stage (Collect StageReproducing) -- Things to apply velocity calculations to
                    -> Lens' Stage (Collect StageReproducing)  -- Things which they may collide with
                    -> CInt                                    -- ticks
                    -> Stage
                    -> Stage
applyVelocityThings usL themL ticks stg =
  let (stg',us') = mapAccumROf traverse
                               (\stgAcc rep -> let (stgAcc',thing') = withLiveClient (rep^.reproducing)
                                                                                     (\c -> applyVelocityThing ticks themL (c^.client) stgAcc)
                                                  in (stgAcc',over reproducing (mapLiveClient (set client thing')) rep)
                               )
                               stg
                               (stg^.usL)
     in set usL us' stg'

-- Apply velocity to a given thing by interleaving 1px movement in each axis.
-- - Checks for collision with the background
-- - Check for collisions and touches against a collection of things.
--  - Applies collision damage
--  - Applies score increase
--  - Remove things which dissapear on contact
applyVelocityThing :: CInt -> Lens' Stage (Collect StageReproducing) -> Thing -> Stage -> (Stage,Thing)
applyVelocityThing ticks oppositionL thing stg =
  let -- Modify the amount to move the thing proportional to the number of ticks
      baseDisplacement                    = thing^.thingVelocity.vel
      displacementModifier                = V2 (fromIntegral ticks / 10) (fromIntegral ticks / 10)
      displacement                        = baseDisplacement * displacementModifier

      -- Try and move the thing, accumulating a list of collisions and touches
      (movedThing,(collisions,touches))   = tryMoveThingByAcc displacement ([],[]) thing validateThingMovement

      -- Total damage of collisions, score increase of touches
      collisionDamage                     = sum . map (_thingContactDamage . snd) $ collisions
      scoreIncrease                       = sum . map (_thingContactScore  . snd) $ touches
      -- Any touched things with "thingContactConsumed" set, should be removed.
      consumedKeys                        = map fst . filter (\(_,cthing) -> cthing^.thingContactConsumed) $ touches

      -- Updated score and thing accounting for damage
      newScore                            = stg^.stageScore + scoreIncrease
      damagedThing                        = over thingHealth (subCounter collisionDamage) movedThing
    in (set stageScore newScore
        . over oppositionL (\c -> foldr deleteKey c consumedKeys) -- remove all consumed things
        $ stg
       ,damagedThing)
  where
    -- Given an accumulated list of keys collided and keys just touching, test whether a thing comes to a stop.
    --
    -- Accumulated keys are also cached alongside the value of their thing when it was determined they collided/ touched.
    validateThingMovement :: ([(Key,Thing)],[(Key,Thing)]) -> Thing -> (Bool,([(Key,Thing)],[(Key,Thing)]))
    validateThingMovement (accCollisions,accTouches) testThing =
      let touchesTileGrid  = touchesStageBackgroundTileGrid stg testThing
          thingCollisions  = stageCollisions oppositionL testThing stg
          collides         = touchesTileGrid || (not . null $ thingCollisions)
          thingTouches     = stageTouches oppositionL testThing stg
         in (not collides,(thingCollisions++accCollisions,thingTouches++accTouches))


-- Apply acceleration due to gravity to the subject
applyGravitySubject :: Stage -> Stage
applyGravitySubject stg = applyForceSubject (stg^.stageGravity) stg

applyGravityUs :: Stage -> Stage
applyGravityUs = applyGravityThings stageUs

applyGravityThem :: Stage -> Stage
applyGravityThem = applyGravityThings stageThem

-- apply gravity to all of the Things
applyGravityThings :: Lens' Stage (Collect StageReproducing) -> Stage -> Stage
applyGravityThings thingsL stg = over (thingsL. traverse) (over reproducing (mapLiveClient (over client (applyForceThing (stg^.stageGravity))))) stg


-- reduce the subjects velocity if it has exceeded the limit
applySpeedLimitSubject :: Stage -> Stage
applySpeedLimitSubject stg = stageSubject.thingVelocity%~limitVelocity (stg^.stageSpeedLimit) $ stg

applySpeedLimitThem :: Stage -> Stage
applySpeedLimitThem = applySpeedLimitThings stageThem

applySpeedLimitUs :: Stage -> Stage
applySpeedLimitUs = applySpeedLimitThings stageUs

-- Reduce all things velocity if it has exceeded the limit
applySpeedLimitThings :: Lens' Stage (Collect StageReproducing) -> Stage -> Stage
applySpeedLimitThings thingsL stg = thingsL     -- Collect StageReproducing
                                  . traverse    -- StageReproducing
                                  . reproducing -- Live Thing Subject ([Reproducing Thing Subject ()],())
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

applyFrictionUs :: Stage -> Stage
applyFrictionUs = applyFrictionThings stageUs

applyFrictionThem :: Stage -> Stage
applyFrictionThem = applyFrictionThings stageThem

-- Apply friction to all things
applyFrictionThings :: Lens' Stage (Collect StageReproducing) -> Stage -> Stage
applyFrictionThings thingsL stg = thingsL -- Collect (Reproducing Thing Pos ())
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


updateUs :: Stage -> Stage
updateUs = updateThings stageUs

updateThem :: Stage -> Stage
updateThem = updateThings stageThem

-- Update each thing by its corresponding agent
updateThings :: Lens' Stage (Collect StageReproducing) -> Stage -> Stage
updateThings thingsL stg
  = let thingInput = stg^.stageSubject
        (newThings,updatedThings) = mapWriteCollect (\k mName repThing0
                                                      -> let (repThing1,(newThings,_)) = updateReproducing thingInput repThing0
                                                            in (newThings,repThing1)
                                                    ) (stg^.thingsL)
       in set thingsL (fst $ insertAnonymouses newThings updatedThings) stg

-- Remove all dead things from the stage collection
removeDeadThings :: Stage -> Stage
removeDeadThings stg =
  let (deads,alives) = partitionCollect (\rep -> withLiveClient (rep^.reproducing) (isDead . _client)) (stg^.stageThem)
     in set stageThem alives stg


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

-- Add a new reproducing thing as being "us" (E.G. a bullet)
addUs :: Maybe Name -> StageReproducing -> Stage -> Stage
addUs mName r stg = over stageUs (fst . insert mName r) stg

-- How many "them" consumables are left?
remainingConsumable :: Stage -> Int
remainingConsumable stg = foldrOf traverse (\rep acc -> if rep^.reproducing.to (\l -> withLiveClient l (_thingContactConsumed . _client)) then acc+1 else acc) 0 (stg^.stageThem)

-- An example client. Handles Text actions, namely walkleft,walkright,jump,shootleft and shootright
stageClient :: Thing       -- relative to thing
            -> StageClient
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
        -> (thing,([bullet (-1) thing],()))

      "shootright"
        -> (thing,([bullet 1 thing],()))

      _
        -> (thing,([],()))

-- An example bullet moving in x
bullet :: CFloat -> Thing -> StageReproducing
bullet x thing = mkReproducing (bulletLive x thing)
  where
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
debugStage = id
{-debugStage stg = traceShow (stg^.stageSubject.thingHealth) stg-}

