module Game.ConfigReader.Config where

import Game.ConfigReader.Arg
import Game.ConfigReader.Option

import Data.Text

data Config = Config
  {_configOptions    :: [Option]
  ,_defaultedOptions :: [DefaultOption]
  }
  deriving (Show)

-- An option defaulted to
data DefaultOption = DefaultOption
  {_defaultOption     :: Text
  ,_defaultIsPositive :: Bool
  ,_defaultArgs       :: [SomeArg]
  }
  deriving (Show,Eq)

