{-# OPTIONS_GHC -freduction-depth=0 #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Termonad.Config where

import Termonad.Prelude

import Control.Lens (makeLensesFor, makePrisms)
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)

import qualified Data.Foldable
import GHC.TypeNats

-- | The font size for the Termonad terminal.  There are two ways to set the
-- fontsize, corresponding to the two different ways to set the font size in
-- the Pango font rendering library.
--
-- If you're not sure which to use, try 'FontSizePoints' first and see how it
-- looks.  It should generally correspond to font sizes you are used to from
-- other applications.
data FontSize
  = FontSizePoints Int
    -- ^ This sets the font size based on \"points\".  The conversion between a
    -- point and an actual size depends on the system configuration and the
    -- output device.  The function 'GI.Pango.fontDescriptionSetSize' is used
    -- to set the font size.  See the documentation for that function for more
    -- info.
  | FontSizeUnits Double
    -- ^ This sets the font size based on \"device units\".  In general, this
    -- can be thought of as one pixel.  The function
    -- 'GI.Pango.fontDescriptionSetAbsoluteSize' is used to set the font size.
    -- See the documentation for that function for more info.
  deriving (Eq, Show)

-- | The default 'FontSize' used if not specified.
--
-- >>> defaultFontSize
-- FontSizePoints 12
defaultFontSize :: FontSize
defaultFontSize = FontSizePoints 12

$(makePrisms ''FontSize)

-- | Settings for the font to be used in Termonad.
data FontConfig = FontConfig
  { fontFamily :: !Text
    -- ^ The font family to use.  Example: @"DejaVu Sans Mono"@ or @"Source Code Pro"@
  , fontSize :: !FontSize
    -- ^ The font size.
  } deriving (Eq, Show)

-- | The default 'FontConfig' to use if not specified.
--
-- >>> defaultFontConfig == FontConfig {fontFamily = "Monospace", fontSize = defaultFontSize}
-- True
defaultFontConfig :: FontConfig
defaultFontConfig =
  FontConfig
    { fontFamily = "Monospace"
    , fontSize = defaultFontSize
    }

$(makeLensesFor
    [ ("fontFamily", "lensFontFamily")
    , ("fontSize", "lensFontSize")
    ]
    ''FontConfig
 )

data INat = Z | S INat

type family FromLit (n :: Nat) :: INat where
  FromLit 0 = 'Z
  FromLit n = 'S (FromLit (n - 1))

data Vec :: INat -> * -> * where
  Empty :: Vec 'Z c
  (:~) :: c -> Vec n c -> Vec ('S n) c

infixr 6 :~

deriving instance Eq c => Eq (Vec n c)
deriving instance Show c => Show (Vec n c)
deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)

data Palette c
  = Palette0
  | Palette8 (Vec (FromLit 8) c)
  | Palette16 (Vec (FromLit 16) c)
  | Palette232 (Vec (FromLit 232) c)
  | Palette256 (Vec (FromLit 256) c)
  deriving (Eq, Show, Functor, Foldable)

paletteToList :: Palette c -> [c]
paletteToList = Data.Foldable.toList

-- | NB: Currently due to issues either with VTE or the bindings generated for
--   Haskell, background colour cannot be set independently of the palette.
--   The @backgroundColour@ field will be ignored and the 0th colour in the
--   palette (usually black) will be used as the background colour.
data ColourConfig c = ColourConfig
  { cursorColour :: !c
  , foregroundColour :: !c
  , backgroundColour :: !c
  , palette :: !(Palette c)
  } deriving (Eq, Show, Functor)

$(makeLensesFor
    [ ("cursorColour", "lensCursorColour")
    , ("foregroundColour", "lensForegroundColour")
    , ("backgroundColour", "lensBackgroundColour")
    , ("palette", "lensPalette")
    ]
    ''ColourConfig
 )

defaultColourConfig :: ColourConfig (Colour Double)
defaultColourConfig = ColourConfig
  { cursorColour = sRGB24 192 192 192 -- lightgrey
  , foregroundColour = sRGB24 192 192 192 -- lightgrey
  , backgroundColour = sRGB24   0   0   0 -- black
  , palette = Palette16
    $  sRGB24   0   0   0 -- 00: black
    :~ sRGB24 192   0   0 -- 01: red
    :~ sRGB24   0 192   0 -- 02: green
    :~ sRGB24 192 192   0 -- 03: yellow
    :~ sRGB24   0   0 192 -- 04: blue
    :~ sRGB24 192   0 192 -- 05: purple
    :~ sRGB24   0 192 192 -- 06: cyan
    :~ sRGB24 192 192 192 -- 07: lightgrey
    :~ sRGB24  63  63  63 -- 08: grey
    :~ sRGB24 255  63  63 -- 09: lightred
    :~ sRGB24  63 255  63 -- 10: lightgreen
    :~ sRGB24 255 255  63 -- 11: lightyellow
    :~ sRGB24  63  63 255 -- 12: lightblue
    :~ sRGB24 255  63 255 -- 13: lightpurple
    :~ sRGB24  63 255 255 -- 14: lightcyan
    :~ sRGB24 255 255 255 -- 15: white
    :~ Empty
  }

data ShowScrollbar
  = ShowScrollbarNever
  | ShowScrollbarAlways
  | ShowScrollbarIfNeeded
  deriving (Eq, Show)

data TMConfig = TMConfig
  { fontConfig :: !FontConfig
  , showScrollbar :: !ShowScrollbar
  , colourConfig :: !(ColourConfig (Colour Double))
  , scrollbackLen :: !Integer
  , confirmExit :: !Bool
  , wordCharExceptions :: !Text
  } deriving (Eq, Show)

$(makeLensesFor
    [ ("fontConfig", "lensFontConfig")
    , ("showScrollbar", "lensShowScrollbar")
    , ("colourConfig", "lensColourConfig")
    , ("scrollbackLen", "lensScrollbackLen")
    , ("confirmExit", "lensConfirmExit")
    , ("wordCharExceptions", "lensWordCharExceptions")
    ]
    ''TMConfig
 )

defaultTMConfig :: TMConfig
defaultTMConfig =
  TMConfig
    { fontConfig = defaultFontConfig
    , showScrollbar = ShowScrollbarIfNeeded
    , colourConfig = defaultColourConfig
    , scrollbackLen = 10000
    , confirmExit = True
    , wordCharExceptions = "-#%&+,./=?@\\_~\183:"
    }
