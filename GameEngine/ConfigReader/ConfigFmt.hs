module GameEngine.ConfigReader.ConfigFmt where

import GameEngine.ConfigReader.OptionFmt
import GameEngine.ConfigReader.Arg

import Data.Text hiding (filter)

data ConfigFmt = ConfigFmt
  {_configFmtOptions  :: [(OptionPairFmt,DefaultFmt)]
  }

-- If no pair in an option is given, default to
data DefaultFmt = DefaultFmt
  {_defaultFmtPositive :: Bool
  ,_defaultFmtArgs     :: [SomeArg]
  }

-- Delete any defaults which we have a given option for
deleteDefault :: Text -> [(OptionPairFmt,DefaultFmt)] -> [(OptionPairFmt,DefaultFmt)]
deleteDefault word = filter ((\pair -> pName pair == word || nName pair == word) . fst) 
  where
    pName = _optionFmtWord . _positiveOption
    nName = _optionFmtWord . _negativeOption

