{-# LANGUAGE GADTs #-}
module GameEngine.ConfigReader where

import GameEngine.ConfigReader.ArgFmt
import GameEngine.ConfigReader.Config
import GameEngine.ConfigReader.ConfigFmt
import GameEngine.ConfigReader.Parser

import Control.Applicative
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Text

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

