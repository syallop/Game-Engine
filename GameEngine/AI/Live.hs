{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , RankNTypes
  #-}
module GameEngine.AI.Live
  (Live(..)
  ,mkLive
  ,update
  ,mapLiveClient
  ,withLiveClient
  )
  where

import Data.Typeable

import GameEngine.AI.Agent
import GameEngine.AI.Client

-- A live thing 't' can:
-- - Make an observation about itself, and then keeping some internal state,
--   decide on an action to perform on itself.
-- 'i' is an external input to the observations
-- 'o' is an output of executing an action.
data Live t i o = forall ob ac. (Typeable ac,Typeable ob,Typeable t) => Live
  {_liveClient :: Client t ob ac o
  ,_liveAgent  :: Agent (i,ob) ac
  }
  deriving Typeable

instance Show t => Show (Live t i o) where
  show (Live c a) = "Live " ++ show c ++ show a

instance (Eq t,Typeable i,Typeable o,Typeable t) => Eq (Live t i o) where
  (Live c0 a0) == (Live c1 a1) = case cast c1 of
    Nothing -> False
    Just c1 -> c0 == c1 && case cast a1 of
                             Nothing -> False
                             Just a1 -> a0 == a1

mkLive :: (Typeable ac,Typeable ob,Typeable t) => Client t ob ac o -> Agent (i,ob) ac -> Live t i o
mkLive c a = Live c a

-- By making an observation about itself decide on an action to perform and perform it.
update :: i -> Live t i o -> (Live t i o,o)
update input (Live c a) =
  let ob      = observe c
      (ac,a') = decideAction (input,ob) a
      (c',o)  = act ac c
     in (Live c' a',o)

mapLiveClient :: forall t i o. (forall ob ac. Client t ob ac o -> Client t ob ac o) -> Live t i o -> Live t i o
mapLiveClient f (Live c a) = Live (f c) a

withLiveClient :: Live t i o -> forall r. (forall ob ac. Client t ob ac o -> r) -> r
withLiveClient (Live c _) f = f c

