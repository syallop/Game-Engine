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

import Game.Position
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
  {_agentState        :: s                   -- Some state
  ,_agentGuardTrigger :: s -> Action -> Bool -- Guard an action from being executed by the state
                                             -- E.G. If we've only just shot a bullet, we need a reload time
  ,_agentRules        :: Rules
  }
  deriving (Typeable)

instance Eq s => Eq (Agent s) where
  (Agent s0 _ rs0) == (Agent s1 _ rs1) = s0 == s1 && rs0 == rs1

instance Show s => Show (Agent s) where
  show (Agent s _ rs) = "Agent " ++ show s ++ " " ++ show rs

mkAgent :: s -> (s -> Action -> Bool) -> Rules -> Maybe (Agent s)
mkAgent s f rs = Just $ Agent s f rs

emptyAgent :: Agent ()
emptyAgent = Agent () (const . const True) $ Map.fromList []

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
  = DistanceLess CFloat
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
  {_observeAgentPosition  :: Pos 
  ,_observePlayerPosition :: Pos
  ,_observeAgentHealth    :: CInt
  ,_observePlayerHealth   :: CInt
  }
makeLenses ''Observe

-- List of actions which triggered
triggerActions :: Observe -> (Action -> Bool) -> Rules -> [Action]
triggerActions o guardF = foldr (\(t,a) acc -> if doesTrigger o t && guardF a then a:acc else acc) [] . Map.toList

-- Does the trigger.. trigger?
doesTrigger :: Observe -> Trigger -> Bool
doesTrigger o t = case t of
  DistanceLess d
    -> distance (o^.observeAgentPosition.pos)
                (o^.observePlayerPosition.pos)
       < d

  AgentHealthHigher h
    -> h < o^.observeAgentHealth

  PlayerHealthHigher h
    -> h < o^.observePlayerHealth

  PlayerLeft
    -> (o^.observePlayerPosition.pos._x) < (o^.observeAgentPosition.pos._x)

  PlayerRight
    -> (o^.observeAgentPosition.pos._x) < (o^.observePlayerPosition.pos._x)

  PlayerUp
    -> (o^.observePlayerPosition.pos._y) < (o^.observeAgentPosition.pos._y)

  PlayerDown
    -> (o^.observeAgentPosition.pos._y) < (o^.observePlayerPosition.pos._y)

  AndT t1 t2
    -> doesTrigger o t1 && doesTrigger o t2

  Not t
    -> not $ doesTrigger o t

  where
    cIntToCFloat :: CInt -> CFloat
    cIntToCFloat = fromIntegral

-- Passed observations about the current state of the world
-- , the agent returns a list of actions to perform.
observe :: Observe -> Agent s -> [Action]
observe o a = triggerActions o (_agentGuardTrigger a (_agentState a)) (_agentRules a)

