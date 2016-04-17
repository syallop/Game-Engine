{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}
module GameEngine.Position
  (Pos(..)
  ,pos
  ,integralPosition
  )
  where

import Control.Lens
import Data.Typeable
import Foreign.C.Types
import Linear

newtype Pos = Pos {_pos :: V2 CFloat}
  deriving (Show,Eq,Num,Fractional,Typeable)
makeLenses ''Pos

-- Position rounded down to a whole number
integralPosition :: Integral i => Pos -> V2 i 
integralPosition (Pos (V2 x y)) = V2 (floor $ x) (floor $ y)

