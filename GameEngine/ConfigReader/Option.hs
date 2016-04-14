module GameEngine.ConfigReader.Option where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Maybe
import Control.Applicative

import GameEngine.ConfigReader.Modifier
import GameEngine.ConfigReader.Arg

-- A concrete option
data Option = Option
    {_optionWord            :: Text      -- The computed option word
    ,_optionIsPositive      :: Bool      -- Is it the positive option?
    ,_optionArgs            :: [SomeArg] -- The required args

    -- The specific modifiers we saw when parsing. Allows us to deduce what the user originally wrote.
    ,_modifiers             :: Modifiers
    }
    deriving (Show,Eq)


