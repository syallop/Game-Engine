{-# LANGUAGE TemplateHaskell #-}
module GameEngine.UI.Txt
  (Txt()
  ,newTxt
  ,openFont
  ,mkTxt

  ,renderTxtSolid

  ,txtText
  ,txtColor
  ,txtFont
  ,txtPos

  ,initText
  ,quitText

  ,FFI.TTFFont
  )
  where

import GameEngine.HitBox
import GameEngine.Position

import Control.Applicative
import Control.Lens
import Data.Text hiding (init)
import Foreign.C.Types
import qualified SDL         as SDL
import qualified SDL.Raw     as Raw
import qualified SDL.TTF     as TTF
import qualified SDL.TTF.FFI as FFI

import Debug.Trace

data Txt = Txt
  {_txtText      :: Text
  ,_txtColor     :: Raw.Color
  ,_txtFont      :: FFI.TTFFont

  ,_txtTexture   :: SDL.Texture
  ,_txtRectangle :: SDL.Rectangle CFloat
  }
makeLenses ''Txt

instance Show Txt where
  show t = "TXT " ++ show (t^.txtText)

txtPos :: Lens' Txt Pos
txtPos = txtRectangle.rectPos

-- intialise the font subsystem
initText :: IO ()
initText = do
  r <- TTF.init
  case r of
    0 -> return ()
    e -> error $ "TTF failed initialisation with code: " ++ show e

-- destroy the font subsystem
quitText :: IO ()
quitText = TTF.quit

-- Open a font at a path
openFont :: FilePath -> Int -> IO FFI.TTFFont
openFont = TTF.openFont


-- Create a Txt object from a path to a font file
newTxt :: Text -> Raw.Color -> FilePath -> Int -> SDL.Rectangle CFloat -> SDL.Renderer -> IO Txt
newTxt text color fontPath pt rect renderer = do
  font <- openFont fontPath pt
  mkTxt text color font rect renderer

-- Make a Txt object from an already loaded TTFFont
mkTxt :: Text -> Raw.Color -> FFI.TTFFont -> SDL.Rectangle CFloat -> SDL.Renderer -> IO Txt
mkTxt text color font rect renderer = do
  surface <- TTF.renderTextSolid font (unpack text) color
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  return $ Txt text color font texture rect


renderTxtSolid :: SDL.Renderer -> Txt -> IO ()
renderTxtSolid renderer txt = SDL.copy renderer (txt^.txtTexture) Nothing $ Just $ floor <$> txt^.txtRectangle

