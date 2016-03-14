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
  )
  where

import Linear
import Foreign.C.Types

import Game.Tile
import Game.Tiles
import Game.Background
import Game.Thing
import Game.Velocity


data Stage t = Stage
  {_background :: Background t
  ,_subject    :: Subject
  ,_things     :: [Thing]
  }
  deriving (Eq,Show)

type Subject = Thing

tickStage :: (Show t,Ord t) => Stage t -> Stage t
tickStage stg = applyVelocitySubject stg

-- Set a stage with a background and a subject, and a list of things
-- TODO: Fail when subject collides with background in starting position.
setStage :: Background t -> Subject -> [Thing] -> Maybe (Stage t)
setStage b s things = Just $ Stage b s things

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

-- Attempt to apply a subjects velocity to its position, not moving it in a direction if
-- it would collide.
-- TODO: Moving by an amount in a direction should still succeed with the maximum allowed amount
-- rather than completly failing
-- TODO: moving right and then down will avoid things we shouldnt
-- TODO: moving by the entire amount at once will jump through obstacles.
{-applyVelocitySubject :: (Show t,Ord t) => Stage t -> Stage t-}
{-applyVelocitySubject stg =-}
  {-let Velocity (V2 vX vY) = _velocity . _subject $ stg-}

      {--- If moving right fails => Hit something => Null the velocity in that direction-}
      {-stg'  = (\stg' -> if stg' == stg-}
                          {-then stg{_subject = mapVelocity nullX (_subject stg)}-}
                          {-else stg'-}
              {-) . moveSubjectRightBy vX $ stg-}
      {--- Same with moving down-}
      {-stg'' = (\stg'' -> if stg'' == stg'-}
                           {-then stg'{_subject = mapVelocity nullY (_subject stg')}-}
                           {-else stg''-}
              {-) . moveSubjectDownBy vY $ stg'-}
     {-in stg''-}


-- Apply velocity to a subject by interleaving moving in either axis.
-- A collision negates velocity in that axis.
applyVelocitySubject :: (Show t,Ord t) => Stage t -> Stage t
applyVelocitySubject stg = applyVelocitySubject' (_vel . _velocity . _subject $ stg) stg
  where
    applyVelocitySubject' (V2 vX vY) stg = interleaveStateful vX vY stg fx fy
      where
        -- Supply the correct x and y function, Down&Right count down, Up&Left count up
        fx = if vX > 0 then fRight else fLeft
        fy = if vY > 0 then fDown  else fUp

        fRight 0 stg = Right stg -- No more movement
        fRight x stg = case moveSubjectRight stg of
                           -- Hit a wall. Null X velocity.
                           Nothing   -> Right $ stg{_subject = mapVelocity nullX (_subject stg)}
                           -- Success! One less step to move
                           Just stg' -> Left (stg',x-1)

        fLeft 0 stg = Right stg -- No more movement
        fLeft x stg = case moveSubjectLeft stg of
                          -- Hit a wall. Null X velocity
                          Nothing   -> Right $ stg{_subject = mapVelocity nullX (_subject stg)}
                          -- Success! One less step to move
                          Just stg' -> Left (stg',x+1)

        fDown 0 stg = Right stg -- No more movement
        fDown y stg = case moveSubjectDown stg of
                           -- Hit a wall. Null X velocity.
                           Nothing   -> Right $ stg{_subject = mapVelocity nullY (_subject stg)}
                           -- Success! One less step to move
                           Just stg' -> Left (stg',y-1)

        fUp 0 stg = Right stg -- No more movement
        fUp y stg = case moveSubjectUp stg of
                           -- Hit a wall. Null X velocity.
                           Nothing   -> Right $ stg{_subject = mapVelocity nullY (_subject stg)}
                           -- Success! One less step to move
                           Just stg' -> Left (stg',y+1)


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

