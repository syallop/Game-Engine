{-# LANGUAGE GADTs #-}
module GameEngine.ConfigReader where

import GameEngine.ConfigReader.ArgFmt
import GameEngine.ConfigReader.Config
import GameEngine.ConfigReader.ConfigFmt
import GameEngine.ConfigReader.Parser

import Prelude hiding (readFile)

import Control.Applicative
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text.IO (readFile)

parseConfigFile :: ConfigFmt -> FilePath -> IO (Either (ParseError Char Dec) Config)
parseConfigFile fmt file = runParser (configP fmt) file <$> readFile file

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

