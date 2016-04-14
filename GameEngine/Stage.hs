{-# LANGUAGE
    ExistentialQuantification
  , TemplateHaskell
  , StandaloneDeriving
  #-} 
module GameEngine.Stage
  (Stage()
  ,Subject(..)
  ,setStage
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  ,stageBackground
  ,stageSubject
  ,stageThings
  ,stageGravity
  ,stageSpeedLimit
  ,stageThingSpeedLimit

  ,tickStage

  ,applyForceSubject
  ,pushForceSubject
  )
  where

import Control.Arrow
import Control.Lens
import Data.Map.Lens
import Data.Text (Text)
import Foreign.C.Types
import Linear hiding (trace)
import SDL
import qualified Data.Map as M

import GameEngine.Agent
import GameEngine.Background
import GameEngine.Collect
import GameEngine.Force
import GameEngine.Thing
import GameEngine.Tile
import GameEngine.TileGrid
import GameEngine.Velocity

import Debug.Trace

type Subject = Thing

data Stage = Stage
  {_stageBackground      :: Background
  ,_stageSubject         :: Subject
  ,_stageThings          :: Collect (Thing,SomeAgent)
  ,_stageGravity         :: Force

  ,_stageSpeedLimit      :: Velocity 
  ,_stageThingSpeedLimit :: Velocity 

  ,_stageSubjectFriction :: CFloat
  ,_stageThingFriction   :: CFloat
  } deriving (Show,Eq)
makeLenses ''Stage


tickStage :: CInt -> Stage -> Stage
tickStage dTicks
  = applyVelocityThings dTicks
  . applySpeedLimitThings
  . applyFrictionThings
  . applyThingsAgents
  . applyGravityThings

  . applyVelocitySubject dTicks
  . applySpeedLimitSubject
  . applyFrictionSubject
  . applyGravitySubject

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background
         -> Subject
         -> Collect (Thing,SomeAgent)
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
         thingFriction     = Just $ Stage b s things gravity subjectSpeedLimit thingSpeedLimit subjectFriction thingFriction

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
     in if collidesTileGrid (effectiveThingHitBox subject') tileGrid
        || collidesThings subject' (map (fst . fst) . collected $ stg^.stageThings)
          then Nothing
          else Just $ set stageSubject subject' stg

-- Does a tile collide with anything on the stage (EXCEPT the subject)?
collidesAnything :: Stage -> Thing -> Bool
collidesAnything stg thing = collidesStageBackgroundTileGrid stg thing
                          || collidesStageThings             stg thing

collidesStageBackgroundTileGrid :: Stage -> Thing -> Bool
collidesStageBackgroundTileGrid stg thing = collidesTileGrid (effectiveThingHitBox thing) (stg^.stageBackground.backgroundTileGrid)

collidesStageThings :: Stage -> Thing -> Bool
collidesStageThings stg thing = collidesThings thing (map (fst . fst) $ collected $ stg^.stageThings)

-- Apply velocity to the subject by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Checks collision with the background and other things.
applyVelocitySubject :: CInt -> Stage -> Stage
applyVelocitySubject ticks stg =
  over stageSubject (\subject -> tryMoveThingBy ((* V2 (fromIntegral ticks / 10) (fromIntegral ticks / 10)) $ stg^.stageSubject.thingVelocity.vel)
                                                (stg^.stageSubject)
                                                (not . collidesAnything stg)) stg

-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: CInt -> Stage -> Stage
applyVelocityThings ticks stg = over (stageThings . traverse) (first applyVelocityThing) stg
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
applyGravityThings stg = over (stageThings . traverse) (first $ applyForceThing (stg^.stageGravity)) stg

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
applySpeedLimitThings stg = stageThings.traverse._1.thingVelocity%~limitVelocity (stg^.stageThingSpeedLimit) $ stg

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
applyFrictionThings stg = stageThings.traverse._1%~applyFrictionThing (stg^.stageThingFriction) $ stg
  where
    applyFrictionThing :: CFloat -> Thing -> Thing
    applyFrictionThing l t
      -- Standing on a tile
      | collidesStageBackgroundTileGrid stg (stg^.stageSubject.to (moveThingBy (V2 0 1)))
       = applyForceThing (t^.thingVelocity.to (opposeX l)) t

      -- Air friction
      | otherwise
       = applyForceThing (t^.thingVelocity.to (opposeX 1)) t


-- Update each thing by its corresponding agent
applyThingsAgents :: Stage -> Stage
applyThingsAgents stg =
  let (newThings,updatedThings) = mapWriteCollect (\_ _ (thing,SomeAgent agent)
                                                    -> let ((thing1,agent1),newThings) = applyThingAgent (thing,agent) (ob thing)
                                                        in (newThings,(thing1,SomeAgent agent1))
                                                  ) (stg^.stageThings)
     in set stageThings (fst $ insertAnonymouses newThings updatedThings) stg
  where
    ob thing = Observe {_observeAgentPosition  = thing^.thingTile.tilePos
                       ,_observePlayerPosition = stg^.stageSubject.thingTile.tilePos
                       ,_observeAgentHealth    = 3
                       ,_observePlayerHealth   = 3
                       }

-- Ask a things agent what to do with a thing under the stage.
-- Return a list of things and agents as the result of doing it.
applyThingAgent :: (Thing,Agent s) -> Observe -> ((Thing,Agent s),[(Thing,SomeAgent)])
applyThingAgent (thing,agent) ob = foldr (\action ((thing0,agent0),newThings0)
                                           -> let ((thing1,agent1),newThings1) = applyActionThing ob action (thing0,agent0)
                                                 in ((thing1,agent1),newThings0++newThings1)
                                         )
                                         ((thing,agent),[])
                                         (observe ob agent)

-- Apply a single action to a thing, returning an updated thing and agent, alongside a list of things and agents to spawn
applyActionThing :: Observe -> Action -> (Thing,Agent s) -> ((Thing,Agent s),[(Thing,SomeAgent)])
applyActionThing ob action (thing,agent) = case action of
  WalkLeft
    -> ((applyForceThing (Force $ V2 (-2) 0) thing,agent),[])

  WalkRight
    -> ((applyForceThing (Force $ V2 2 0) thing,agent),[])

  Jump
    -> ((applyForceThing (Force $ V2 0 (-5)) thing,agent),[])

  Spawn f
    -> ((thing,agent),[f ob])

  And a1 a2
    -> let ((thing1,agent1),newThings1) = applyActionThing ob a1 (thing,agent)
           ((thing2,agent2),newThings2) = applyActionThing ob a2 (thing1,agent1)
          in ((thing2,agent2),newThings1 ++ newThings2)

  Or a1 a2
    -> applyActionThing ob a1 (thing,agent)

