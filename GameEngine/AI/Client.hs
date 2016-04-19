{-# LANGUAGE
    DeriveDataTypeable
  , TemplateHaskell
  #-}
module GameEngine.AI.Client
  (Client(..)
  ,mkClient
  ,emptyClient

  ,observe
  ,act

  ,client
  ,clientObservations
  ,clientUpdate
  )
  where

import Control.Lens
import Data.Typeable

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

