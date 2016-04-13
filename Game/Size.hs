{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Game.Size where

import Control.Lens
import Foreign.C.Types
import Linear

newtype Size = Size {_size :: V2 CFloat}
  deriving (Show,Eq,Num)
makeLenses ''Size

integralSize :: Integral i => Size -> V2 i
integralSize (Size (V2 x y)) = V2 (floor $ x) (floor $ y)

