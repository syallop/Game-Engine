module Game.Stage
  (Stage()
  ,Subject(..)
  ,setStage
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  ,stageBackground
  ,stageBackgroundTiles
  ,stageBackgroundImage
  ,stageSubjectTile
  ,stageUnitSize

  ,things
  {-,applyVelocitySubject-}
  ,tickStage

  ,applyForceSubject
  ,pushForceSubject
  )
  where

import SDL
import Linear
import Foreign.C.Types
import Control.Arrow

import Game.Tile
import Game.Tiles
import Game.Background
import Game.Thing
import Game.Velocity
import Game.Force
import Game.Agent

import Debug.Trace

data Stage t = Stage
  {_background :: Background t
  ,_subject    :: Subject
  ,_things     :: [(Thing,Agent)]

  ,_gravity    :: Force
  ,_speedLimit :: V2 CInt
  ,_thingSpeedLimit :: V2 CInt
  }
  deriving (Eq,Show)

type Subject = Thing

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
moveSubjectLeft  = moveSubjectLeftBy 1
moveSubjectDown  = moveSubjectDownBy 1
moveSubjectUp    = moveSubjectUpBy 1
-- Move a subject in a direction by a positive amount if they do not collide
-- with the background
moveSubjectRightBy, moveSubjectLeftBy, moveSubjectDownBy, moveSubjectUpBy :: (Show t,Ord t) => CInt -> Stage t -> Maybe (Stage t)
moveSubjectRightBy x = mapSubjectTile (moveR x)
moveSubjectLeftBy  x = mapSubjectTile (moveL x)
moveSubjectDownBy  y = mapSubjectTile (moveD y)
moveSubjectUpBy    y = mapSubjectTile (moveU y)

-- Map a function across the subjects tile IF the resulting tile does not
-- collide with the background
mapSubjectTile :: (Show t,Ord t) => (Tile -> Tile) -> Stage t -> Maybe (Stage t)
mapSubjectTile f stg =
  let tile     = _thingTile . _subject $ stg
      nextTile = f tile
     in setSubjectTile nextTile stg

-- Set the subject to the given tile, if it does not collide with
-- the background or any of the things
setSubjectTile :: (Show t,Ord t) => Tile -> Stage t -> Maybe (Stage t)
setSubjectTile tile stg =
  let background = backgroundTiles . _background $ stg
      subject    = _subject stg
     in if collidesTiles tile background
        || collidesThings tile (map fst . things $ stg)
          then Nothing
          else Just $ stg{_subject = subject{_thingTile = tile}}

stageBackground :: Stage t -> Background t
stageBackground = _background

stageBackgroundTiles :: Stage t -> Tiles t
stageBackgroundTiles = backgroundTiles . _background

stageBackgroundImage :: Stage t -> Maybe Texture
stageBackgroundImage = backgroundImage . _background

stageSubjectTile :: Stage t -> Tile
stageSubjectTile = _thingTile . _subject

things :: Stage t -> [(Thing,Agent)]
things = _things

stageUnitSize :: Stage t -> CInt
stageUnitSize = _tileUnitSize . stageBackgroundTiles

-- Does a tile collide with anything on the stage (EXCEPT the subject)?
collidesAnything :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesAnything stg tile = collidesStageBackground stg tile
                         || collidesStageThings     stg tile

collidesStageBackground :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageBackground stg tile = collidesTiles tile (backgroundTiles . _background $ stg)

collidesStageThings :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageThings stg tile = collidesThings tile (map fst . _things $ stg)

-- Apply velocity to the subject by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Checks collision with the background and other things.
applyVelocitySubject :: (Show t,Ord t) => CInt -> Stage t -> Stage t
applyVelocitySubject ticks stg =
  stg{_subject = tryMoveThingBy ((* (V2 ticks ticks)) . _vel . _velocity . _subject $ stg)
                                (_subject stg)
                                (not . collidesAnything stg)
     }

-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: (Show t,Ord t) => CInt -> Stage t -> Stage t
applyVelocityThings ticks stg =
  stg{_things = map (first applyVelocityThing) (_things stg)
     }
  where
    applyVelocityThing :: Thing -> Thing
    applyVelocityThing thing = tryMoveThingBy ((* (V2 ticks ticks)) . _vel . _velocity $ thing) thing (not . collidesStageBackground stg)

-- Apply acceleration due to gravity to the subject
applyGravitySubject :: Stage t -> Stage t
applyGravitySubject stg = applyForceSubject (_gravity stg) stg

-- apply gravity to all of the Things
applyGravityThings :: Stage t -> Stage t
applyGravityThings stg =
  stg{_things = map (first $ applyForceThing (_gravity stg)) (_things stg)
     }

-- Apply a force to a subject to change its velocity
applyForceSubject :: Force -> Stage t -> Stage t
applyForceSubject force stg =
  stg{_subject = applyForceThing force $ _subject stg
     }

-- Apply force to a subject, only if it is making contact with a solid object in the opposite
-- direction with which to 'push' off of.
pushForceSubject :: (Show t,Ord t) => Force -> Stage t -> Stage t
pushForceSubject force stg
  | collidesStageBackground stg (thingTile . moveThingBy (V2 x y) . _subject $ stg) = applyForceSubject force stg
  | otherwise = stg

  where
    x = if isPositive $ xComponent force then -1 else 1
    y = if isPositive $ yComponent force then -1 else 1

    isPositive = (>= 0)

-- reduce the subjects velocity if it has exceeded the limit
applySpeedLimitSubject :: Stage t -> Stage t
applySpeedLimitSubject stg = stg{_subject = mapVelocity (limitVelocity (_speedLimit stg)) (_subject stg)}

-- Reduce all things velocity if it has exceeded the limit
applySpeedLimitThings :: Stage t -> Stage t
applySpeedLimitThings stg = stg{_things = map (first (applySpeedLimitThing (_thingSpeedLimit stg))) (_things stg)}
  where
    applySpeedLimitThing :: V2 CInt -> Thing -> Thing
    applySpeedLimitThing l = mapVelocity (limitVelocity l)

-- Apply friction to the subject
applyFrictionSubject :: (Show t,Ord t) => Stage t -> Stage t
applyFrictionSubject stg
  -- Standing on a tile
  | collidesStageBackground stg (thingTile . moveThingBy (V2 0 1) . _subject $ stg) = applyForceSubject (opposeX 2 (_velocity . _subject $ stg)) stg

  -- Less air friction
  | otherwise = applyForceSubject (opposeX 1 (_velocity . _subject $ stg)) stg
  {-| otherwise = applyForceSubject (opposeX 1 (_velocity . _subject $ stg)) stg-}

-- Update each thing by its corresponding agent
applyThingsAgents :: Stage t -> Stage t
applyThingsAgents stg =
  let things  = _things stg
      things' = map (\(t,a) -> (applyThingAgent (t,a) stg,a)) things
     in stg{_things = things'}

-- Ask a things agent what to do with a thing, then do it.
applyThingAgent :: (Thing,Agent) -> Stage t -> Thing
applyThingAgent (thing,agent) stg =
  let ob = Observe {_observeAgentPosition  = (\t -> V2 (posX t) (posY t)) . _thingTile $ thing
                   ,_observePlayerPosition = (\t -> V2 (posX t) (posY t)) . _thingTile . _subject $ stg
                   ,_observeAgentHealth    = 3
                   ,_observePlayerHealth   = 3
                   }
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

