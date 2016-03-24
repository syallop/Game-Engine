module Game.Agent
  (Agent()
  ,mkAgent
  ,emptyAgent
  ,observe

  ,Action(..)
  ,Trigger(..)
  ,Observe(..)

  ,exAgent
  )
  where

import Control.Lens
import Foreign.C.Types
import Linear hiding (trace)
import Data.Maybe
import qualified Data.Map as Map

-- Something an agent may decide to do
data Action
  = WalkLeft
  | WalkRight
  | Jump
  | And Action Action
  | Or Action Action
  deriving (Eq,Show)

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
  {_observeAgentPosition  :: V2 CInt
  ,_observePlayerPosition :: V2 CInt
  ,_observeAgentHealth    :: CInt
  ,_observePlayerHealth   :: CInt
  }

-- List of actions which triggered
triggerActions :: Observe -> Rules -> [Action]
triggerActions o = foldr (\(t,a) acc -> if doesTrigger o t then a:acc else acc) [] . Map.toList

-- Does the trigger.. trigger?
doesTrigger :: Observe -> Trigger -> Bool
doesTrigger o t = case t of
  DistanceLess d
    -> (distance (fmap cIntToCFloat . _observeAgentPosition $ o) (fmap cIntToCFloat . _observePlayerPosition $ o)) < (cIntToCFloat d)

  AgentHealthHigher h
    -> h < _observeAgentHealth o

  PlayerHealthHigher h
    -> h < _observePlayerHealth o

  PlayerLeft
    -> (view _x . _observePlayerPosition $ o) < (view _x . _observeAgentPosition $ o)

  PlayerRight
    -> (view _x . _observeAgentPosition $ o) < (view _x . _observePlayerPosition $ o)

  PlayerUp
    -> (view _y . _observePlayerPosition $ o) < (view _y . _observeAgentPosition $ o)

  PlayerDown
    -> (view _y . _observeAgentPosition $ o) < (view _y . _observePlayerPosition $ o)

  AndT t1 t2
    -> doesTrigger o t1 && doesTrigger o t2

  Not t
    -> not $ doesTrigger o t

  where
    cIntToCFloat :: CInt -> CFloat
    cIntToCFloat = fromIntegral

data Agent = Agent
  {_rules :: Rules
  }
  deriving (Eq,Show)

mkAgent :: Rules -> Maybe Agent
mkAgent = Just . Agent

emptyAgent :: Agent
emptyAgent = Agent $ Map.fromList []

-- Passed observations about the current state of the world
-- , the agent returns a list of actions to perform.
observe :: Observe -> Agent -> [Action]
observe o a = triggerActions o (_rules a)

-- near player && high health => walk nearer player
-- near player && low  health => walk further player
-- attackingdistance player && high health => attack player
-- under player => jump
exAgent :: Agent
exAgent = fromJust $ mkAgent $ Map.fromList
  {-[(DistanceLess 128,Jump)-}
  {-]-}
  [((DistanceLess 256) `AndT` PlayerLeft,WalkLeft)
  ,((DistanceLess 256) `AndT` PlayerRight,WalkRight)
  ]
