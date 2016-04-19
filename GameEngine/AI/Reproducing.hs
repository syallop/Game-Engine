{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module GameEngine.AI.Reproducing
  (Reproducing(..)
  ,mkReproducing
  ,updateReproducing
  ,reproducing
  )
  where

import GameEngine.AI.Agent
import GameEngine.AI.Live

import Control.Lens
import Data.Typeable

-- A 'Live t i' that produces a list of similarly typed Live things aswell as an output from
-- performing its actions.
newtype Reproducing t i o = Reproducing {_reproducing :: Live t i ([Reproducing t i o],o)}
  deriving (Show,Eq,Typeable)

mkReproducing :: Live t i ([Reproducing t i o],o) -> Reproducing t i o
mkReproducing = Reproducing

updateReproducing :: i -> Reproducing t i o -> (Reproducing t i o,([Reproducing t i o],o))
updateReproducing i (Reproducing l) = let (l',o) = update i l in (Reproducing l',o)

reproducing :: Lens' (Reproducing t i o) (Live t i ([Reproducing t i o],o))
reproducing = lens (\(Reproducing r) -> r) (\(Reproducing _) l -> Reproducing l)

