{-# LANGUAGE TemplateHaskell, GADTs , StandaloneDeriving,ScopedTypeVariables,DeriveDataTypeable #-}
module Game.Agent
  (Agent()
  ,mkAgent
  ,emptyAgent
  ,observe

  ,Action(..)
  ,Trigger(..)
  ,Observe(..)
  ,observeAgentPosition
  ,observePlayerPosition
  ,observeAgentHealth
  ,observePlayerHealth

  ,SomeAgent(..)
  )
  where

import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Foreign.C.Types
import Linear hiding (trace)
import Linear.Affine
import qualified Data.Map as Map

import Game.Thing

data SomeAgent = forall s. (Show s,Eq s,Typeable s) => SomeAgent (Agent s)

instance Eq SomeAgent where
  s0 == s1 = case (s0,s1) of
    (SomeAgent (a0 :: Agent s0),SomeAgent (a1 :: Agent s1))
      -> case cast a1 :: Maybe (Agent s0) of
          Nothing -> False
          Just a2 -> a0 == a2

instance Show SomeAgent where
  show (SomeAgent a) = show a


data Agent s = Agent
  {_agentState :: s
  ,_agentRules :: Rules
  }
  deriving (Eq,Show,Typeable)

-- Something an agent may decide to do
data Action
  = WalkLeft
  | WalkRight
  | Jump
  | And Action Action
  | Or Action Action
  | Spawn (Observe -> (Thing,SomeAgent))
  -- | ModifyState

instance Show Action where
  show a = case a of
    WalkLeft  -> "WalkLeft"
    WalkRight -> "WalkRight"
    Jump      -> "Jump"
    And a0 a1 -> "And " ++ show a0 ++ " " ++ show a1
    Or a0 a1  -> "Or " ++ show a0 ++ " " ++ show a1
    Spawn _   -> "Spawn"

instance Eq Action where
  a0 == a1 = case (a0,a1) of
    (WalkLeft,WalkLeft)   -> True
    (WalkRight,WalkRight) -> True
    (Jump,Jump)           -> True
    (And a0 a1,And a2 a3) -> a0 == a2 && a1 == a3
    (Or a0 a1,Or a2 a3)   -> a0 == a2 && a1 == a3
    (Spawn _,Spawn _)     -> True
    _                     -> False

-- Something which may trigger an agent to do something
data Trigger
  = DistanceLess CInt
  | AgentHealthHigher CInt
  | PlayerHealthHigher CInt
  | PlayerLeft
  | PlayerRight
  | PlayerUp
  | PlayerDown
  | Not Trigger
  | AndT Trigger Trigger
  deriving (Eq,Show,Ord)

-- Map triggers which when satisfied, trigger an action
type Rules = Map.Map Trigger Action

-- Information the agent observes to determine any triggers
data Observe = Observe
  {_observeAgentPosition  :: Point V2 CInt
  ,_observePlayerPosition :: Point V2 CInt
  ,_observeAgentHealth    :: CInt
  ,_observePlayerHealth   :: CInt
  }
makeLenses ''Observe

-- List of actions which triggered
triggerActions :: Observe -> Rules -> [Action]
triggerActions o = foldr (\(t,a) acc -> if doesTrigger o t then a:acc else acc) [] . Map.toList

-- Does the trigger.. trigger?
doesTrigger :: Observe -> Trigger -> Bool
doesTrigger o t = case t of
  DistanceLess d
    -> distance (fmap cIntToCFloat $ o^.observeAgentPosition.lensP)
                (fmap cIntToCFloat $ o^.observePlayerPosition.lensP)
       < cIntToCFloat d

  AgentHealthHigher h
    -> h < o^.observeAgentHealth

  PlayerHealthHigher h
    -> h < o^.observePlayerHealth

  PlayerLeft
    -> (o^.observePlayerPosition.lensP._x) < (o^.observeAgentPosition.lensP._x)

  PlayerRight
    -> (o^.observeAgentPosition.lensP._x) < (o^.observePlayerPosition.lensP._x)

  PlayerUp
    -> (o^.observePlayerPosition.lensP._y) < (o^.observeAgentPosition.lensP._y)

  PlayerDown
    -> (o^.observeAgentPosition.lensP._y) < (o^.observePlayerPosition.lensP._y)

  AndT t1 t2
    -> doesTrigger o t1 && doesTrigger o t2

  Not t
    -> not $ doesTrigger o t

  where
    cIntToCFloat :: CInt -> CFloat
    cIntToCFloat = fromIntegral


mkAgent :: s -> Rules -> Maybe (Agent s)
mkAgent s rs = Just $ Agent s rs

emptyAgent :: Agent ()
emptyAgent = Agent () $ Map.fromList []

-- Passed observations about the current state of the world
-- , the agent returns a list of actions to perform.
observe :: Observe -> Agent s -> [Action]
observe o a = triggerActions o (_agentRules a)

