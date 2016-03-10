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
  )
  where

import Foreign.C.Types

import Game.Tile
import Game.Tiles
import Game.Background
import Game.Thing

data Subject = Subject
  {_subjectTile :: Tile
  }
  deriving Show

data Stage t = Stage
  {_background :: Background t
  ,_subject    :: Subject
  ,_things     :: [Thing]
  }
  deriving Show

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background t -> Subject -> [Thing] -> Maybe (Stage t)
setStage b s things = Just $ Stage b s things

-- Move a subject in a direction if they do not collide with the background
moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp :: (Show t,Ord t) => Stage t -> Stage t 
moveSubjectRight = moveSubjectRightBy 1
moveSubjectLeft  = moveSubjectLeftBy 1
moveSubjectDown  = moveSubjectDownBy 1
moveSubjectUp    = moveSubjectUpBy 1

-- Move a subject in a direction by a positive amount if they do not collide
-- with the background
moveSubjectRightBy, moveSubjectLeftBy, moveSubjectDownBy, moveSubjectUpBy :: (Show t,Ord t) => CInt -> Stage t -> Stage t
moveSubjectRightBy x = mapSubjectTile (moveR x)
moveSubjectLeftBy  x = mapSubjectTile (moveL x)
moveSubjectDownBy  y = mapSubjectTile (moveD y)
moveSubjectUpBy    y = mapSubjectTile (moveU y)

-- Map a function across the subjects tile IF the resulting tile does not
-- collide with the background
mapSubjectTile :: (Show t,Ord t) => (Tile -> Tile) -> Stage t -> Stage t
mapSubjectTile f stg =
  let tile     = _subjectTile . _subject $ stg
      nextTile = f tile
     in setSubjectTile nextTile stg

-- Set the subject to the given tile, if it does not collide with
-- the background or any of the things
setSubjectTile :: (Show t,Ord t) => Tile -> Stage t -> Stage t
setSubjectTile tile stg =
  let background = backgroundTiles . _background $ stg
      subject    = _subject stg
     in if collidesTiles tile background
        || collidesThings tile (things stg)
          then stg
          else stg{_subject = subject{_subjectTile = tile}}

stageBackgroundTiles :: Stage t -> Tiles t
stageBackgroundTiles = backgroundTiles . _background

stageSubjectTile :: Stage t -> Tile
stageSubjectTile = _subjectTile . _subject

things :: Stage t -> [Thing]
things = _things

