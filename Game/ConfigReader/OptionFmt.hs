module Game.ConfigReader.OptionFmt where

import Game.ConfigReader.ArgFmt

import Data.Text

newtype OptionPairsFmt = OptionPairsFmt [OptionPairFmt]

-- OptionPair format. A 'positive' option and a contrasting negative option.
-- The key word for each option may be prefixed with:
-- - A PositiveModifier which is just for readability and has no effect on the meaning.
-- - A NegativeModifier which changes the meaning to the opposite word.
-- E.G. Given "colored" and "colorless" as the positive and negative words respectively,
--      each with no args, then:
-- |-----Read-------|---Means-----|
-- |"colored"       | "colored"   |
-- |"colorless"     | "colorless" |
-- |"is colored"    | "colored"   |
-- |"is colorless"  | "colorless" |
-- |"isnt colored"  | "colorless" |
-- |"not colorless" | "colored"   |
--
-- The expected arg format comes the deduced option. Multiple args are space seperated.
-- E.G. If "colored" expects three ArgFmtInt's then:
-- |-----Read----------------|---Means-----------|
-- |"is colored 255 0 255"   | colored 255 0 255 |
-- |"not colorless 255 0 255 | colored 255 0 255 |
data OptionPairFmt = OptionPairFmt
  {_positiveOption :: OptionFmt
  ,_negativeOption :: OptionFmt
  }

-- Option format. A key word and then zero or many arguments are expected in an order
data OptionFmt = OptionFmt
  {_optionFmtWord :: Text
  ,_optionFmtArgs :: [SomeArgFmt]
  }

