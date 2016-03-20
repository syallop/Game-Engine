{-# LANGUAGE GADTs #-}
module Game.ConfigReader.Parser where

import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Modifier
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt
import Game.ConfigReader.Modifier

import Data.Text hiding (map,foldr)
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Maybe
import Data.List

import Control.Applicative

configP :: ConfigFmt -> Parser Config
configP configFmt = do
  options <- many ((choice $ map (try . optionP . fst) $ _configFmtOptions configFmt) <* newline)
  let defaultOptions :: [DefaultOption]
      defaultOptions = map mkDefault $
                         foldr (\opt acc -> deleteDefault (_optionWord opt) acc)
                               (_configFmtOptions $ configFmt)
                               options
  pure $ Config (nub options) (nub defaultOptions)
  where
    mkDefault :: (OptionPairFmt,DefaultFmt) -> DefaultOption
    mkDefault (optPair,def) =
      if _defaultFmtPositive def
        then DefaultOption (_optionFmtWord . _positiveOption $ optPair)
                           True
                           (_defaultFmtArgs def)
        else DefaultOption (_optionFmtWord . _negativeOption $ optPair)
                           False
                           (_defaultFmtArgs def)

optionP :: OptionPairFmt -> Parser Option
optionP optionPairFmt = do
  -- Parse any possible modifiers
  mods <- modifiersP

  -- Parse either of the words
  eWord <- eitherWordP optionPairFmt

  -- If the modifiers were negative, the opposite word was meant.
  -- Note whether the meant word is the positive variant.
  let (meantWord,isPositive)
          = if modifiersPositive mods
              then case eWord of
                       Left  w -> (w,False)
                       Right w -> (w,True)
              else case eWord of
                       Left  _ -> (_optionFmtWord . _positiveOption $ optionPairFmt,True)
                       Right _ -> (_optionFmtWord . _negativeOption $ optionPairFmt,False)

  -- We now expect the args of the meant word, NOT the actual word
  as <- someArgsP $ if modifiersPositive mods
                      then (_optionFmtArgs . _positiveOption $ optionPairFmt)
                      else (_optionFmtArgs . _negativeOption $ optionPairFmt)

  pure $ Option meantWord isPositive as mods
  where
    eitherWordP :: OptionPairFmt -> Parser (Either Text Text)
    eitherWordP optionPairFmt = eitherP (pack <$> (string' . unpack . _optionFmtWord . _negativeOption $ optionPairFmt))
                                        (pack <$> (string' . unpack . _optionFmtWord . _positiveOption $ optionPairFmt))

modifiersP :: Parser Modifiers
modifiersP = many (modifierP <* space)

modifierP :: Parser Modifier
modifierP =  isP
         <|> doesP
         <|> try haveP
         <|> hasP
         <|> try notP
         <|> noP
  where
    isP   = (pure $ Right PIs)   <* string' "is"
    doesP = (pure $ Right PDoes) <* string' "does"
    haveP = (pure $ Right PHave) <* string' "have"
    hasP  = (pure $ Right PHas)  <* string' "has"
    notP  = (pure $ Left NNot)   <* string' "not"
    noP   = (pure $ Left NNo)    <* string' "no"

someArgsP :: [SomeArgFmt] -> Parser [SomeArg]
someArgsP []                        = pure []
someArgsP ((SomeArgFmt aFmt):aFmts) = (:) <$> someArgP aFmt <*> (space *> someArgsP aFmts)

someArgP :: ArgFmt t -> Parser SomeArg
someArgP argFmt = SomeArg <$> argP argFmt

argP :: ArgFmt t -> Parser (Arg t)
argP argFmt = case argFmt of
  ArgFmtInt  -> argIntP
  ArgFmtBool -> argBoolP
  ArgFmtText -> argTextP

argIntP :: Parser (Arg Int)
argIntP = ArgInt . read <$> some digitChar

argBoolP,trueP,falseP :: Parser (Arg Bool)
argBoolP = trueP <|> falseP

trueP  = pure (ArgBool True)  <* (string' "true" <|> string' "yes" <|> string "1")
falseP = pure (ArgBool False) <* (string' "false" <|> string' "no" <|> string "0")

argTextP :: Parser (Arg Text)
argTextP = ArgText . pack <$> (string "\"" *> manyTill anyChar (string "\""))

