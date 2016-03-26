{-# LANGUAGE TemplateHaskell #-}
module Game.Stage
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
  ,stageBackgroundTiles
  ,stageBackgroundImage
  ,stageUnitSize

  {-,applyVelocitySubject-}
  ,tickStage

  ,applyForceSubject
  ,pushForceSubject
  )
  where

import Control.Arrow
import Control.Lens
import Foreign.C.Types
import Linear
import SDL

import Game.Agent
import Game.Background
import Game.Force
import Game.Thing
import Game.Tile
import Game.Tiles
import Game.Velocity

import Debug.Trace

type Subject = Thing

data Stage t = Stage
  {_stageBackground      :: Background t
  ,_stageSubject         :: Subject
  ,_stageThings          :: [(Thing,Agent)]
  ,_stageGravity         :: Force
  ,_stageSpeedLimit      :: V2 CInt
  ,_stageThingSpeedLimit :: V2 CInt
  }
  deriving (Eq,Show)

makeLenses ''Stage


tickStage :: (Show t,Ord t) => CInt -> Stage t -> Stage t
tickStage dTicks
  = applyVelocityThings dTicks
  . applySpeedLimitThings
  . applyThingsAgents
  . applyGravityThings

  . applyVelocitySubject dTicks
  . applySpeedLimitSubject
  . applyFrictionSubject
  . applyGravitySubject

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background t -> Subject -> [(Thing,Agent)] -> Force -> Maybe (Stage t)
setStage b s things gravity = Just $ Stage b s things gravity (V2 5 20) (V2 4 20)

-- Move a subject in a direction if they do not collide with the background
moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp :: (Show t,Ord t) => Stage t -> Maybe (Stage t)
moveSubjectRight = moveSubjectRightBy 1
moveSubjectLeft  = moveSubjectLeftBy  1
moveSubjectDown  = moveSubjectDownBy  1
moveSubjectUp    = moveSubjectUpBy    1
-- Move a subject in a direction by a positive amount if they do not collide
-- with the background
moveSubjectRightBy, moveSubjectLeftBy, moveSubjectDownBy, moveSubjectUpBy :: (Show t,Ord t) => CInt -> Stage t -> Maybe (Stage t)
moveSubjectRightBy x = mapSubjectTile (moveTileR x)
moveSubjectLeftBy  x = mapSubjectTile (moveTileL x)
moveSubjectDownBy  y = mapSubjectTile (moveTileD y)
moveSubjectUpBy    y = mapSubjectTile (moveTileU y)

-- Map a function across the subjects tile IF the resulting tile does not
-- collide with the background
mapSubjectTile :: (Show t,Ord t) => (Tile -> Tile) -> Stage t -> Maybe (Stage t)
mapSubjectTile f stg =
  let tile     = stg^.stageSubject.thingTile
      nextTile = f tile
     in setSubjectTile nextTile stg

-- Set the subject to the given tile, if it does not collide with
-- the background or any of the things
setSubjectTile :: (Show t,Ord t) => Tile -> Stage t -> Maybe (Stage t)
setSubjectTile tile stg =
  let background = stg^.stageBackgroundTiles
      subject    = stg^.stageSubject
     in if collidesTiles tile background
        || collidesThings tile (map fst $ stg^.stageThings)
          then Nothing
          else Just $ set (stageSubject.thingTile) tile stg

stageBackgroundTiles :: Lens' (Stage t) (Tiles t)
stageBackgroundTiles = stageBackground.backgroundTiles

stageBackgroundImage :: Lens' (Stage t) (Maybe Texture)
stageBackgroundImage = stageBackground . backgroundImage

stageUnitSize :: Lens' (Stage t) CInt
stageUnitSize = stageBackgroundTiles.tilesUnitSize

-- Does a tile collide with anything on the stage (EXCEPT the subject)?
collidesAnything :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesAnything stg tile = collidesStageBackground stg tile
                         || collidesStageThings     stg tile

collidesStageBackground :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageBackground stg tile = collidesTiles tile (stg^.stageBackground.backgroundTiles)

collidesStageThings :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageThings stg tile = collidesThings tile (map fst $ stg^.stageThings)

-- Apply velocity to the subject by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Checks collision with the background and other things.
applyVelocitySubject :: (Show t,Ord t) => CInt -> Stage t -> Stage t
applyVelocitySubject ticks stg =
  over stageSubject (\subject -> tryMoveThingBy ((* (V2 ticks ticks)) $ stg^.stageSubject.thingVelocity.vel)
                                                (stg^.stageSubject)
                                                (not . collidesAnything stg)) stg


  {-stg{_stageSubject = tryMoveThingBy ((* (V2 ticks ticks)) . _vel . _thingVelocity . _stageSubject $ stg)-}
                                {-(stg^.stageSubject)-}
                                {-(not . collidesAnything stg)-}
     {-}-}

-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: (Show t,Ord t) => CInt -> Stage t -> Stage t
applyVelocityThings ticks stg =
  stg{_stageThings = map (first applyVelocityThing) (_stageThings stg)
     }
  where
    applyVelocityThing :: Thing -> Thing
    applyVelocityThing thing = tryMoveThingBy ((* (V2 ticks ticks)) . _vel . _thingVelocity $ thing) thing (not . collidesStageBackground stg)

-- Apply acceleration due to gravity to the subject
applyGravitySubject :: Stage t -> Stage t
applyGravitySubject stg = applyForceSubject (_stageGravity stg) stg

-- apply gravity to all of the Things
applyGravityThings :: Stage t -> Stage t
applyGravityThings stg =
  stg{_stageThings = map (first $ applyForceThing (_stageGravity stg)) (_stageThings stg)
     }

-- Apply a force to a subject to change its velocity
applyForceSubject :: Force -> Stage t -> Stage t
applyForceSubject force stg =
  stg{_stageSubject = applyForceThing force $ _stageSubject stg
     }

-- Apply force to a subject, only if it is making contact with a solid object in the opposite
-- direction with which to 'push' off of.
pushForceSubject :: (Show t,Ord t) => Force -> Stage t -> Stage t
pushForceSubject force stg
  | collidesStageBackground stg (_thingTile . moveThingBy (V2 x y) $ stg^.stageSubject) = applyForceSubject force stg
  | otherwise = stg

  where
    x = if isPositive $ xComponent force then -1 else 1
    y = if isPositive $ yComponent force then -1 else 1

    isPositive = (>= 0)

-- reduce the subjects velocity if it has exceeded the limit
applySpeedLimitSubject :: Stage t -> Stage t
applySpeedLimitSubject stg = stg{_stageSubject = over thingVelocity (limitVelocity (_stageSpeedLimit stg)) (_stageSubject stg)}

-- Reduce all things velocity if it has exceeded the limit
applySpeedLimitThings :: Stage t -> Stage t
applySpeedLimitThings stg = stg{_stageThings = map (first (applySpeedLimitThing (_stageSpeedLimit stg))) (_stageThings stg)}
  where
    applySpeedLimitThing :: V2 CInt -> Thing -> Thing
    applySpeedLimitThing l = over thingVelocity (limitVelocity l)

-- Apply friction to the subject
applyFrictionSubject :: (Show t,Ord t) => Stage t -> Stage t
applyFrictionSubject stg
  -- Standing on a tile
  | collidesStageBackground stg (_thingTile $ moveThingBy (V2 0 1) $ stg^.stageSubject) = applyForceSubject (opposeX 2 $ stg^.stageSubject.thingVelocity) stg


  -- Less air friction
  | otherwise = applyForceSubject (opposeX 1 $ stg^.stageSubject.thingVelocity) stg

-- Update each thing by its corresponding agent
applyThingsAgents :: Stage t -> Stage t
applyThingsAgents stg =
  let things  = _stageThings stg
      things' = map (\(t,a) -> (applyThingAgent (t,a) stg,a)) things
     in stg{_stageThings = things'}

-- Ask a things agent what to do with a thing, then do it.
applyThingAgent :: (Thing,Agent) -> Stage t -> Thing
applyThingAgent (thing,agent) stg =
  let ob = Observe {_observeAgentPosition  = thing^.thingTile.tilePos
                   ,_observePlayerPosition = stg^.stageSubject.thingTile.tilePos
                   ,_observeAgentHealth    = 3
                   ,_observePlayerHealth   = 3
                   }
  {-let ob = Observe {_observeAgentPosition  = (\t -> V2 (posX t) (posY t)) . _thingTile $ thing-}
                   {-,_observePlayerPosition = (\t -> V2 (posX t) (posY t)) . _thingTile . _subject $ stg-}
                   {-,_observeAgentHealth    = 3-}
                   {-,_observePlayerHealth   = 3-}
                   {-}-}
      actions = observe ob agent
     in foldr applyActionThing thing actions

-- Apply a single action to a thing
applyActionThing :: Action -> Thing -> Thing
applyActionThing a t = case a of
  WalkLeft
    -> applyForceThing (Force $ V2 (-1) 0) t

  WalkRight
    -> applyForceThing (Force $ V2 1 0) t

  Jump
    -> applyForceThing (Force $ V2 0 (-5)) t

  And a1 a2
    -> applyActionThing a2 . applyActionThing a1 $ t

  Or a1 a2
    -> applyActionThing a1 t

