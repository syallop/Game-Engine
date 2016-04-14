{-# LANGUAGE
    ExistentialQuantification
  , GADTs
  , StandaloneDeriving
  #-}
module GameEngine.ConfigReader.Arg where

import Data.Text

-- Some Arg ignoring its tagged type.
data SomeArg = forall t. SomeArg (Arg t)

instance Eq SomeArg where
  (SomeArg a0) == (SomeArg a1) = case (a0,a1) of
    (ArgInt   _,ArgInt   _) -> a0 == a1
    (ArgFloat _,ArgFloat _) -> a0 == a1
    (ArgChar  _,ArgChar  _) -> a0 == a1
    (ArgBool  _,ArgBool  _) -> a0 == a1
    (ArgText  _,ArgText  _) -> a0 == a1
    (ArgList l0,ArgList l1) -> listEq l0 l1

listEq :: [Arg t] -> [Arg t'] -> Bool
listEq [] [] = True
listEq _  [] = False
listEq [] _  = False
listEq (l:ls) (r:rs) = SomeArg l == SomeArg r && listEq ls rs

instance Show SomeArg where
  show (SomeArg a) = "SomeArg " ++ show a

-- An argument with type 't'.
data Arg t where
  ArgInt   :: Int     -> Arg Int
  ArgFloat :: Float   -> Arg Float
  ArgChar  :: Char    -> Arg Char
  ArgBool  :: Bool    -> Arg Bool
  ArgText  :: Text    -> Arg Text
  ArgList  :: Eq t => [Arg t] -> Arg [t]

deriving instance Eq t => Eq (Arg t)
instance Show (Arg t) where
  show a = case a of
    ArgInt   i -> "ArgInt "   ++ show i
    ArgFloat f -> "ArgFloat " ++ show f
    ArgBool  b -> "ArgBool "  ++ show b
    ArgChar  c -> "ArgChar "  ++ show c
    ArgText  t -> "ArgText "  ++ show t
    ArgList  l -> "ArgList "  ++ show l

