module Game.ConfigReader.Modifier where

import Data.Either

-- A negative modifier word.
data NegativeModifier
  = NNo
  | NNot
  deriving (Show,Eq)

-- A positive modifier word.
data PositiveModifier
  = PIs
  | PHas
  | PHave
  | PDoes
  | PUses
  deriving (Show,Eq)

-- Either a negative or a positive modifier.
type Modifier = Either NegativeModifier PositiveModifier

-- A list of Modifiers.
type Modifiers = [Modifier]

-- Does a list of modifiers indicate the positive option?
modifiersPositive :: Modifiers -> Bool
modifiersPositive = (== 0) . (`mod` 2) . length . lefts

-- Does a list of modifiers indicate the negative option?
modifiersNegative :: Modifiers -> Bool
modifiersNegative = not . modifiersPositive

