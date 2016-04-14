module GameEngine.ConfigReader.Config where

import GameEngine.ConfigReader.Arg
import GameEngine.ConfigReader.Option

import Data.Maybe
import Data.Text hiding (map)

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

-- Is an option set, either explicitly or by default?
isSet :: Text -> Config -> Bool
isSet w config = elem w $ (map _optionWord . _configOptions $ config) ++ (map _defaultOption . _defaultedOptions $ config)

-- Assuming an option is set, either explicitly or by default, return its args.
getArgs' :: Text -> Config -> [SomeArg]
getArgs' w config = fromJust $ getArgs w config

-- If an option is set, either explicitly or by default, return its args.
getArgs :: Text -> Config -> Maybe [SomeArg]
getArgs w config = lookup w $ (map (\o -> (_optionWord o   ,_optionArgs  o)) . _configOptions    $ config)
                           ++ (map (\d -> (_defaultOption d,_defaultArgs d)) . _defaultedOptions $ config)

args :: Text -> [SomeArg] -> Config -> [SomeArg]
args w def config = maybe def id $ getArgs w config

fromArgs :: Text -> ([SomeArg] -> t) -> t -> Config -> t
fromArgs w f defT config = maybe defT f $ getArgs w config

