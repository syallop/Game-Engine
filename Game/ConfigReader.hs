module Game.ConfigReader where

import Game.ConfigReader.Parser
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt

import Text.Megaparsec
import Text.Megaparsec.Text

parseConfigFile :: ConfigFmt -> FilePath -> IO (Either ParseError Config)
parseConfigFile fmt = parseFromFile (configP fmt)

