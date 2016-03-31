{-# LANGUAGE TemplateHaskell #-}
module Game.TileSet
  (TileSet()
  ,mkTileSet

  ,lookupTileType
  ,memberTileName
  ,tileNames
  )
  where

import Game.Tile

import Control.Lens
import Data.Text
import qualified Data.Map as M

-- Map Text labels to a TileType
newtype TileSet = TileSet {_tileSetMapping :: M.Map Text TileType} deriving (Eq,Show)
makeLenses ''TileSet

mkTileSet :: M.Map Text TileType -> TileSet
mkTileSet = TileSet

lookupTileType :: Text -> TileSet -> Maybe TileType
lookupTileType tName tileSet = M.lookup tName $ tileSet^.tileSetMapping

memberTileName :: Text -> TileSet -> Bool
memberTileName tName tileSet = M.member tName (tileSet^.tileSetMapping)

tileNames :: TileSet -> [Text]
tileNames tileSet = M.keys $ tileSet^.tileSetMapping

