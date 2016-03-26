module Game.Counter
  (Counter()
  ,mkCounter

  ,resetCounter
  ,addCounter
  ,getCount
  )
  where

import Foreign.C.Types

-- A counter keeps a CInt between a range
data Counter = Counter
  {_count        :: CInt
  ,_initialCount :: CInt
  ,_minCount     :: CInt
  ,_maxCount     :: CInt
  }
  deriving (Show,Eq)

mkCounter :: CInt -- ^ Initial count
          -> CInt -- ^ Minimum count
          -> CInt -- ^ Maximum count
          -> Maybe Counter -- ^ minimum <= initial <= maximum
mkCounter iC minC maxC
  | (minC <= iC) && (iC <= maxC) && (minC <= maxC) = Just $ Counter iC iC minC maxC

resetCounter :: Counter -> Counter
resetCounter c = c{_count = _initialCount c}

-- Add an amount to a counter. Dont exceed the minimum or maximum.
addCounter :: CInt -> Counter -> Counter
addCounter x c
  | _count c + x < _minCount c  = c{_count = _minCount c}
  | _maxCount c  < _count c + x = c{_count = _maxCount c}
  | otherwise                   = c{_count = _count c + x}

getCount :: Counter -> CInt
getCount = _count

