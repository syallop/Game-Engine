{-# LANGUAGE GADTs, ExistentialQuantification #-}
module Game.ConfigReader.ArgFmt where

import Data.Text

-- Some Argument format ignoring its type
data SomeArgFmt = forall t. SomeArgFmt (ArgFmt t)

-- Argument format. An arg is expected with the type 't'.
data ArgFmt t where
  ArgFmtInt   :: ArgFmt Int
  ArgFmtFloat :: ArgFmt Float
  ArgFmtChar  :: ArgFmt Char
  ArgFmtBool  :: ArgFmt Bool
  ArgFmtText  :: ArgFmt Text

