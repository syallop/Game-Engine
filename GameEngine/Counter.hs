{-# LANGUAGE TemplateHaskell #-}
module GameEngine.Counter
  (Counter()
  ,mkCounter

  ,resetCounter
  ,addCounter

  ,counterCount
  ,counterInitialCount
  ,counterMinCount
  ,counterMaxCount
  )
  where

import Control.Lens
import Foreign.C.Types

-- A counter keeps a CInt between a range
data Counter = Counter
  {_counterCount        :: CInt
  ,_counterInitialCount :: CInt
  ,_counterMinCount     :: CInt
  ,_counterMaxCount     :: CInt
  }
  deriving (Show,Eq)

makeLenses ''Counter

mkCounter :: CInt -- ^ Initial count
          -> CInt -- ^ Minimum count
          -> CInt -- ^ Maximum count
          -> Maybe Counter -- ^ minimum <= initial <= maximum
mkCounter iC minC maxC
  | (minC <= iC) && (iC <= maxC) && (minC <= maxC) = Just $ Counter iC iC minC maxC

resetCounter :: Counter -> Counter
resetCounter c = set counterCount (c^.counterInitialCount) c

-- Add an amount to a counter. Dont exceed the minimum or maximum.
addCounter :: CInt -> Counter -> Counter
addCounter x c
  | c^.counterCount + x < c^.counterMinCount  = set counterCount (c^.counterMinCount) c
  | c^.counterMaxCount  < c^.counterCount + x = set counterCount (c^.counterMaxCount) c
  | otherwise                                 = over counterCount (+x) c

