{-# LANGUAGE GADTs, ExistentialQuantification, StandaloneDeriving #-}
module Game.ConfigReader.Arg where

import Data.Text

-- Some Arg ignoring its tagged type.
data SomeArg = forall t. SomeArg (Arg t)

instance Eq SomeArg where
  (SomeArg a0) == (SomeArg a1) = case (a0,a1) of
    (ArgInt  _,ArgInt  _) -> a0 == a1
    (ArgBool _,ArgBool _) -> a0 == a1
    (ArgText _,ArgText _) -> a0 == a1

instance Show SomeArg where
  show (SomeArg a) = "SomeArg " ++ show a

-- An argument with type 't'.
data Arg t where
  ArgInt  :: Int  -> Arg Int
  ArgBool :: Bool -> Arg Bool
  ArgText :: Text -> Arg Text

deriving instance Eq t => Eq (Arg t)
instance Show (Arg t) where
  show a = case a of
    ArgInt  i -> "ArgInt "  ++ show i
    ArgBool b -> "ArgBool " ++ show b
    ArgText t -> "ArgText " ++ show t

