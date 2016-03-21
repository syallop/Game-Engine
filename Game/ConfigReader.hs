{-# LANGUAGE GADTs #-}
module Game.ConfigReader where

import Game.ConfigReader.Parser
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt

import Text.Megaparsec
import Text.Megaparsec.Text

import Control.Applicative
import System.Directory

parseConfigFile :: ConfigFmt -> FilePath -> IO (Either ParseError Config)
parseConfigFile fmt = parseFromFile (configP fmt)

-- NAME.{EXTENSION}
extension :: FilePath -> String
extension = drop 1 . dropWhile (/= '.')

-- {NAME}.EXTENSION
name :: FilePath -> String
name = takeWhile (/= '.')

-- List all the non-special (.,..) entries in a directory
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

