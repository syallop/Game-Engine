module Game.Stage
  (Stage()
  ,Subject(..)
  ,Background(..)
  ,setStage
  ,moveSubjectRight,moveSubjectLeft,moveSubjectDown,moveSubjectUp
  ,moveSubjectRightBy,moveSubjectLeftBy,moveSubjectDownBy,moveSubjectUpBy
  ,mapSubjectTile
  ,setSubjectTile

  ,stageBackgroundTiles
  ,stageSubjectTile
  )
  where

import Foreign.C.Types

import Game.Tile
import Game.Tiles

data Subject = Subject
  {_subjectTile :: Tile
  }

data Background t = Background
  {_backgroundTiles :: Tiles t 
  }

data Stage t = Stage
  {_background :: Background t
  ,_subject    :: Subject
  }

-- Set a stage with a background and a subject.
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background t -> Subject -> Maybe (Stage t)
setStage b s = Just $ Stage b s

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
-- the background
setSubjectTile :: (Show t,Ord t) => Tile -> Stage t -> Stage t
setSubjectTile tile stg =
  let background = _backgroundTiles . _background $ stg
      subject    = _subject stg
     in if collides tile background
          then stg
          else stg{_subject = subject{_subjectTile = tile}}

stageBackgroundTiles :: Stage t -> Tiles t
stageBackgroundTiles = _backgroundTiles . _background

stageSubjectTile :: Stage t -> Tile
stageSubjectTile = _subjectTile . _subject

