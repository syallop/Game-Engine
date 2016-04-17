{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , ImpredicativeTypes
  , TemplateHaskell
  #-}
module GameEngine.Live
  (Agent()
  ,mkAgent
  ,emptyAgent
  ,constAgent
  ,decideAction
  ,SomeAgent(..)
  ,withSomeAgent

  ,Client(..)
  ,mkClient
  ,emptyClient

  ,client
  ,clientObservations
  ,clientUpdate

  ,Live(..)
  ,mkLive
  ,update
  ,mapLiveClient
  ,withLiveClient

  ,Reproducing(..)
  ,mkReproducing
  ,updateReproducing
  ,reproducing
  )
  where

import Control.Lens
import Data.Typeable

data SomeAgent = forall ob ac. SomeAgent (Agent ob ac)

withSomeAgent :: SomeAgent -> forall r. (forall ob ac. Agent ob ac -> r) -> r
withSomeAgent (SomeAgent a) f = f a

-- An agent has an internal state, and an action function which
-- takes this state and an observation and produces an action and replaces the state.
data Agent ob ac = forall st. (Show st,Eq st,Typeable st) => Agent
  {_agentState  :: st                  -- Internal state of an agent
  ,_agentDecide :: ob -> st -> (ac,st) -- Given an observation and the internal state, produce an action and a new state
  }
  deriving Typeable
makeLenses ''Agent
instance Show (Agent ob ac) where
  show (Agent st _) = "Agent " ++ show st
instance Eq (Agent ob ac) where
  (Agent st0 _) == (Agent st1 _) = case cast st1 of
    Nothing  -> False
    Just st1 -> st0 == st1

mkAgent :: (Show st,Eq st,Typeable st) => st -> (ob -> st -> (ac,st)) -> Agent ob ac
mkAgent initialState actF = Agent initialState actF

emptyAgent :: Agent () ()
emptyAgent = mkAgent () (\() () -> ((),()))

-- An agent which always and only returns the same action
constAgent :: ac -> Agent ob ac
constAgent ac = mkAgent () (\_ () -> (ac,()))

-- Given an observation, decide on an action to perform, also updating the Agents own state.
decideAction :: ob -> Agent ob ac -> (ac,Agent ob ac)
decideAction ob (Agent st actF) = let (ac,st') = actF ob st in (ac,Agent st' actF)


-- A client is a type 't' that can:
-- - Be observed to produce 'ob'
-- - Be updated by an action 'ac' to produce a new 't'.
data Client t ob ac o = Client
  {_client             :: t                -- The client
  ,_clientObservations :: t -> ob          -- An observation type. Properties an agent can inspect
  ,_clientUpdate       :: ac -> t -> (t,o) -- Update a client by an action, producing some output
  }
  deriving Typeable
makeLenses ''Client
instance Show t => Show (Client t ob ac o) where
  show (Client t _ _) = "Client " ++ show t
instance Eq t => Eq (Client t ob ac o) where
  (Client t0 _ _) == (Client t1 _ _) = t0 == t1

mkClient :: t -> (t -> ob) -> (ac -> t -> (t,o)) -> Client t ob ac o
mkClient thing observeThing updateThing = Client thing observeThing updateThing

emptyClient :: t -> Client t () () ()
emptyClient t0 = mkClient t0 (const ()) (\() t -> (t,()))

-- Make an observation about the current state of the Client
observe :: Client t ob ac o -> ob
observe (Client c obF _) = obF c

-- Given an action, apply it to the Client
act :: ac -> Client t ob ac o -> (Client t ob ac o,o)
act ac (Client t obF upF) = let (t',o) = upF ac t in (Client t' obF upF,o)


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

