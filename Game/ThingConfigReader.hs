{-# LANGUAGE
    GADTs
  , TupleSections
  , OverloadedStrings
  #-}
module Game.ThingConfigReader where

import Game.ConfigReader
import Game.ConfigReader.Arg
import Game.ConfigReader.ArgFmt
import Game.ConfigReader.Config
import Game.ConfigReader.ConfigFmt
import Game.ConfigReader.Option
import Game.ConfigReader.OptionFmt

import Game.Counter
import Game.HitBox
import Game.Thing
import Game.Tile
import Game.TileSet
import Game.Velocity

import Control.Applicative
import Control.Lens
import Data.Maybe
import Data.Monoid
import Data.Text hiding (filter,foldr)
import Foreign.C.Types
import Linear
import Linear.Affine (Point(..))
import SDL
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
parseThings :: FilePath -> TileSet -> CInt -> IO Things
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

parseThing :: FilePath -> FilePath -> CInt -> TileSet -> IO (Maybe Thing)
parseThing thingFile thingsPath radius tileset = do
  res <- parseConfigFile thingConfigFmt (thingsPath ++ "/" ++ thingFile)
  case res of
    -- Failed to parse thing config file
    Left _
      -> return Nothing

    Right thingConfig
      -> do let tileName = fromArgs "tile" (\[SomeArg (ArgText tileName)] -> tileName) "" thingConfig
                defaultPosition = P $ V2 0 0

            -- Attempt to load the named tile.
            -- Cache against the tiletype so we can inherit properties from the tile like solidness
            let mTile :: Maybe (TileType,Tile)
                mTile = case lookupTileType tileName tileset of
                          -- Not found
                          Nothing
                            -- Because no name was specified. Use an invisible, non solid tile
                            | tileName == "" -> Just (TileTypeInvisible False,mkTile (TileTypeInvisible False) (Rectangle defaultPosition (V2 radius radius)))
                            -- Because it doesnt exist in the tileset
                            | otherwise      -> Nothing

                          -- tiletype found, create an instance
                          Just tileType
                            -> Just (tileType,mkTile tileType (Rectangle defaultPosition (V2 radius radius)))

            -- If we managed to create a tile, build the thing from the config options
            -- and the properties of the tile.
            case mTile of
              -- Failed to find the desired tile, fail!
              Nothing
                -> return Nothing

              Just (tileType,tile)
                -> let isSolid         = tileType^.tileTypeIsSolid
                       hasMass         = isSet "mass" thingConfig
                       defaultVelocity = Velocity (V2 0 0)
                       defaultCounter  = fromJust $ mkCounter 3 0 3
                      in return $ Just $ Thing tile isSolid hasMass defaultVelocity defaultCounter NoHitBox

