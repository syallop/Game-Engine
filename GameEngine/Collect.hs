{-# LANGUAGE
    DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , OverloadedStrings
  , TemplateHaskell
  #-}
module GameEngine.Collect
  (Collect()
  ,Name(..)
  ,Key()
  ,emptyCollect
  ,mkCollect
  ,collect

  {- Membership -}
  ,memberName
  ,memberKey
  ,nullCollect

  {- Lookups -}
  ,lookupKey
  ,lookupName
  ,getKey

  {- Insertions -}
  ,insert
  ,insertNamed
  ,insertAnonymous
  ,insertAnonymouses

  {- Deletions -}
  ,freeName
  ,deleteName
  ,deleteKey

  {- Mapping and Misc -}
  ,mapCollect
  ,mapAccumCollect
  ,mapWriteCollect
  ,foldCollect
  ,collected
  ,partitionCollect
  ,keys
  )
  where

import qualified Data.Map    as M
import qualified Data.IntMap as IM
import Control.Arrow
import Control.Lens
import Data.Coerce
import Data.Foldable (Foldable)
import Data.List hiding (insert,null)
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text (Text)

newtype Key  = Key  {_key  :: IM.Key} deriving (Show,Eq)
newtype Name = Name {_name :: Text} deriving (Show,Eq,Ord,IsString)

data Collect t = Collect
  {_collectThings     :: IM.IntMap (t,Maybe Name) -- Map key to a thing, caching a possible name.
  ,_collectNameKeys   :: M.Map Name Key           -- Map names to keys
  ,_collectCurrentKey :: Key                      -- Track the last assigned Key
  }
  deriving (Show,Eq,Traversable,Foldable,Functor)
makeLenses ''Collect

emptyCollect :: Collect t
emptyCollect = Collect IM.empty M.empty (Key 0)

mkCollect :: [(t,Name)] -> [t] -> Collect t
mkCollect named anonymous =
  let namedThings     = map (second Just) named
      anonymousThings = map (\t -> (t,Nothing)) anonymous
      allThings       = namedThings ++ anonymousThings
      lastKey         = length allThings
      allThingsMap    = [1..] `zip` allThings
      nameKeys        = map snd named `zip` map Key [1..]
     in Collect (IM.fromList allThingsMap) (M.fromList nameKeys) (Key lastKey)

collect :: [(t,Maybe Name)] -> Collect t
collect ns =
  let (named,anonymous) = partition (isJust . snd) ns
     in mkCollect (map (\(t,Just n) -> (t,n)) named) (map fst anonymous)

{- Membership -}
memberKey :: Key -> Collect t -> Bool
memberKey k = IM.member (_key k) . _collectThings

memberName :: Name -> Collect t -> Bool
memberName n = M.member n . _collectNameKeys

nullCollect :: Collect t -> Bool
nullCollect = List.null . keys

{- Lookups -}
lookupKey :: Key -> Collect t -> Maybe (t,Maybe Name)
lookupKey k c = IM.lookup (coerce k) $ c^.collectThings

lookupName :: Name -> Collect t -> Maybe (t,Key)
lookupName n c = do
  k <- M.lookup n (c^.collectNameKeys)
  (t,_) <- IM.lookup (_key k) (c^.collectThings)
  return (t,k)

getKey :: Key -> Collect t -> (t,Maybe Name)
getKey k c = fromJust $ lookupKey k c


{- Inserting -}

insert :: Maybe Name -> t -> Collect t -> (Collect t,Key)
insert mName = case mName of
  Nothing   -> insertAnonymous
  Just name -> insertNamed name

insertNamed :: Name -> t -> Collect t -> (Collect t,Key)
insertNamed n t c =
  let nextKey = Key $ _key (c^.collectCurrentKey) + 1
    in ( over collectThings (IM.insert (_key nextKey) (t,Just n))
         . set collectCurrentKey nextKey
         . over collectNameKeys (M.insert n nextKey)
         $ c
       ,nextKey
       )

-- Insert a single anonymous thing, return its key
insertAnonymous :: t -> Collect t -> (Collect t,Key)
insertAnonymous t c=
  let nextKey = Key $ _key (c^.collectCurrentKey) + 1
     in (over collectThings (IM.insert (_key nextKey) (t,Nothing)) . set collectCurrentKey nextKey $ c,nextKey)

-- Insert many anonymous things, returning a list of their keys
insertAnonymouses :: [t] -> Collect t -> (Collect t,[Key])
insertAnonymouses ts c = foldr (\t (c,ks) -> let (c',k) = insertAnonymous t c in (c',k:ks)) (c,[]) ts

{- Deleting -}

-- If a name is assigned to a thing, unassign the name leaving the thing anonymous
freeName :: Name -> Collect t -> Collect t
freeName n c = case M.lookup n (c^.collectNameKeys) of
  -- No Such name
  Nothing -> c

  -- Make the old thing anonymous/ only accessable by the key
  Just k
    -> over collectThings (IM.adjust (\(t,Just _) -> (t,Nothing)) (_key k))
       . over collectNameKeys (M.delete n)
       $ c

-- Delete the target of a name
deleteName :: Name -> Collect t -> Collect t
deleteName n c = case M.lookup n (c^.collectNameKeys) of
  -- No Such name
  Nothing -> c

  Just k
    -> over collectThings (IM.delete (_key k)) . over collectNameKeys (M.delete n) $ c

-- Delete the target of a key, also removing its name if it had one.
deleteKey :: Key -> Collect t -> Collect t
deleteKey (Key k) c = case IM.lookup k (c^.collectThings) of
  -- No such key
  Nothing -> c

  Just (t,mName)
    -> (case mName of
          Nothing   -> id
          Just name -> over collectNameKeys (M.delete name)
       ) . over collectThings (IM.delete k) $ c

{- Mapping & misc -}

-- Map a function over every thing in the Collect, with access to the key and possible name
mapCollect :: (Key -> Maybe Name -> t -> t) -> Collect t -> Collect t
mapCollect f = over collectThings (IM.mapWithKey (\k (t,mName) -> (f (Key k) mName t,mName)))

-- Map a function with an accumulator over every thing in the Collect, with access to the key and possible name
mapAccumCollect :: (acc -> Key -> Maybe Name -> t -> (acc,t)) -> acc -> Collect t -> (acc,Collect t)
mapAccumCollect f acc collect =
  let things = collect^.collectThings
      (acc',things') = imapAccumR (\k acc (t,mName) -> let (acc',t') = f acc (Key k) mName t in (acc',(t',mName)))
                                  acc
                                  things
     in (acc',set collectThings things' collect)

-- Map a function over everything in the Collect, with access to the key and possible name, also emitting an monoid 'm' a which is
-- finally mConcat'd
mapWriteCollect :: Monoid m => (Key -> Maybe Name -> t -> (m,t)) -> Collect t -> (m,Collect t)
mapWriteCollect f collect =
  let (accs,collect') = mapAccumCollect (\accs k mName t -> let (acc,t') = f k mName t in (acc:accs,t')) [] collect
     in (mconcat accs,collect')

foldCollect :: (Key -> Maybe Name -> t -> a -> a) -> a -> Collect t -> a
foldCollect f acc c = IM.foldWithKey (\k (t,mName) acc -> f (Key k) mName t acc) acc (c^.collectThings)

collected :: Collect t -> [(t,Maybe Name)]
collected (Collect t _ _) = IM.elems t

-- All of the keys
keys :: Collect t -> [Key]
keys (Collect t _ _) = map Key $ IM.keys t

partitionCollect :: (t -> Bool) -> Collect t -> (Collect t,Collect t)
partitionCollect f c = let (ts,fs) = partition (\(t,_) -> f t) $ collected c
                          in (collect ts,collect fs)

