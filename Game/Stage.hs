module Game.Stage
  (Stage()
  ,Subject(..)
  ,setStage
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  ,stageBackgroundTiles
  ,stageSubjectTile

  ,things
  {-,applyVelocitySubject-}
  ,tickStage

  ,applyForceSubject
  )
  where

import Linear
import Foreign.C.Types

import Game.Tile
import Game.Tiles
import Game.Background
import Game.Thing
import Game.Velocity
import Game.Force

import Debug.Trace

data Stage t = Stage
  {_background :: Background t
  ,_subject    :: Subject
  ,_things     :: [Thing]

  ,_gravity    :: Force
  }
  deriving (Eq,Show)

type Subject = Thing

tickStage :: (Show t,Ord t) => Stage t -> Stage t
tickStage = applyVelocityThings . applyGravityThings . applyVelocitySubject . applyGravitySubject

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background t -> Subject -> [Thing] -> Force -> Maybe (Stage t)
setStage b s things gravity = Just $ Stage b s things gravity

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
        || collidesThings tile (things stg)
          then Nothing
          else Just $ stg{_subject = subject{_thingTile = tile}}

stageBackgroundTiles :: Stage t -> Tiles t
stageBackgroundTiles = backgroundTiles . _background

stageSubjectTile :: Stage t -> Tile
stageSubjectTile = _thingTile . _subject

things :: Stage t -> [Thing]
things = _things

-- Does a tile collide with anything on the stage (EXCEPT the subject)?
collidesAnything :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesAnything stg tile = collidesStageBackground stg tile
                         || collidesStageThings     stg tile

collidesStageBackground :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageBackground stg tile = collidesTiles tile (backgroundTiles . _background $ stg)

collidesStageThings :: (Show t,Ord t) => Stage t -> Tile -> Bool
collidesStageThings stg tile = collidesThings tile (_things stg)

-- Apply velocity to the subject by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Checks collision with the background and other things.
applyVelocitySubject :: (Show t,Ord t) => Stage t -> Stage t
applyVelocitySubject stg =
  stg{_subject = tryMoveThingBy (_vel . _velocity . _subject $ stg)
                                (_subject stg)
                                (not . collidesAnything stg)
     }

-- Apply velocity to the things by interleaving 1px movement in each axis.
-- Hiting an obstacle in one axis negates velocity in that axis. Movement in the other may continue.
-- Only checks collision with the background, not the subject or other things.
applyVelocityThings :: (Show t,Ord t) => Stage t -> Stage t
applyVelocityThings stg =
  stg{_things = map applyVelocityThing (_things stg)
     }
  where
    applyVelocityThing :: Thing -> Thing
    applyVelocityThing thing = tryMoveThingBy (_vel . _velocity $ thing) thing (not . collidesStageBackground stg)

-- Apply acceleration due to gravity to the subject
applyGravitySubject :: Stage t -> Stage t
applyGravitySubject stg = applyForceSubject (_gravity stg) stg

applyGravityThings :: Stage t -> Stage t
applyGravityThings stg =
  stg{_things = map (applyForceThing (_gravity stg)) (_things stg)
     }

applyForceSubject :: Force -> Stage t -> Stage t
applyForceSubject force stg =
  stg{_subject = applyForceThing force $ _subject stg
     }

