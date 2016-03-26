{-# LANGUAGE GADTs, TupleSections, OverloadedStrings #-}
module Game.ThingConfigReader where

import Game.ConfigReader
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt

import Game.Tile
import Game.Tiles
import Game.Velocity
import Game.Thing
import Game.Counter

import Linear
import Linear.Affine (Point(..))
import Foreign.C.Types

import Data.Maybe
import Data.Monoid
import Data.Text hiding (filter,foldr)
import Control.Applicative
import qualified Data.Map as Map

thingConfigFmt :: ConfigFmt
thingConfigFmt = ConfigFmt
  [(OptionPairFmt (OptionFmt "tile"      [SomeArgFmt ArgFmtText])
                  (OptionFmt "emptyTile" [])
                  ,DefaultFmt True       [SomeArg $ ArgText "emptyTile"])

  ,(OptionPairFmt (OptionFmt "mass"     [])
                  (OptionFmt "massless" [])
                  ,DefaultFmt True      [])
  ]

-- Given a path to a directory of thing files and a tileset the things may use,
-- create the things!
parseThings :: FilePath -> TileSet Text -> CInt -> IO Things
parseThings thingsPath tileset radius = do
  files <- listDirectory thingsPath

  -- Get the files with a ".thing" extension
  let thingFiles = filter ((== "thing") . extension) files

  -- Associate the NAME of all thing files to their parsed Thing
  -- mThings :: [(Text,Maybe Thing)]
  mThings <- mapM (\thingFile -> (pack . name $ thingFile,) <$> parseThing thingFile thingsPath radius tileset) thingFiles

  -- Silently drop all where the config file failed to parse
  let things :: [(Text,Thing)]
      things = foldr (\(name,mThing) acc -> case mThing of
                                              Nothing    -> acc
                                              Just thing -> (name,thing):acc
                     )
                     []
                     mThings

  return $ Map.fromList things

parseThing :: FilePath -> FilePath -> CInt -> TileSet Text -> IO (Maybe Thing)
parseThing thingFile thingsPath radius tileset = do
  res <- parseConfigFile thingConfigFmt (thingsPath ++ "/" ++ thingFile)
  case res of
    -- Failed to parse thing config file
    Left _
      -> return Nothing

    Right thingConfig
      -> do tileName <- if isSet "tile" thingConfig
                          -- tile name is specified, extract the name
                          then case getArgs "tile" thingConfig of
                                   [SomeArg (ArgText tileName)] -> return tileName
                                   _                            -> error "Didnt parse as claimed"

                          -- no name given. "" is not returned by the parser so we can use it here
                          -- to denote no name until we figure out what to do about that
                          else return ""

            let defaultPosition = P $ V2 0 0

            -- Attempt to load the named tile.
            -- Cache against the info so we can inherit properties from the tile like solidness
            let mTile :: Maybe (TileInfo,Tile)
                mTile = case Map.lookup tileName tileset of
                          -- Not found
                          Nothing
                            -- Because no name was specified. Use an invisible, non solid tile
                            | tileName == "" -> Just $ (InfoInvisible False,invisibleTile defaultPosition radius)
                            -- Because it doesnt exist in the tileset
                            | otherwise      -> Nothing

                          -- tileInfo found, create an instance
                          Just tileInfo
                            -> Just (tileInfo,tileInfoInstance tileInfo defaultPosition radius)

            -- If we managed to create a tile, build the thing from the config options
            -- and the properties of the tile.
            case mTile of
              -- Failed to find the desired tile, fail!
              Nothing
                -> return Nothing

              Just (tileInfo,tile)
                -> let isSolid         = _tileInfoIsSolid tileInfo
                       hasMass         = isSet "mass" thingConfig
                       defaultVelocity = Velocity (V2 0 0)
                       defaultCounter  = fromJust $ mkCounter 3 0 3
                      in return $ Just $ Thing tile isSolid hasMass defaultVelocity defaultCounter

