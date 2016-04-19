{-# LANGUAGE
    DeriveDataTypeable
  , ExistentialQuantification
  , RankNTypes
  , TemplateHaskell
  #-}
module GameEngine.AI.Agent
  (Agent()
  ,mkAgent
  ,emptyAgent
  ,constAgent

  ,decideAction

  ,SomeAgent(..)
  ,withSomeAgent
  )
  where

import Control.Lens
import Data.Typeable

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

data SomeAgent = forall ob ac. SomeAgent (Agent ob ac)

withSomeAgent :: SomeAgent -> forall r. (forall ob ac. Agent ob ac -> r) -> r
withSomeAgent (SomeAgent a) f = f a

