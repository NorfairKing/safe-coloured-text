{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Text.Colour.Code where

import Data.ByteString (ByteString)
import Data.List
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Builder.Int as LTB
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Text ()
import Data.Word
import GHC.Generics (Generic)

-- https://en.wikipedia.org/wiki/Escape_character#ASCII_escape_character
asciiEscape :: Char
asciiEscape = '\ESC'

csiStart :: Char
csiStart = '['

csiDelimiter :: Char
csiDelimiter = ';'

newtype CSI
  = SGR [SGR]
  deriving (Show, Eq, Generic)

instance Validity CSI

-- | Render a CSI directly to 'ByteString' using UTF8.
--
-- You probably want to use 'renderCSI' instead.
-- This is just for testing.
renderCSIUtf8BS :: CSI -> ByteString
renderCSIUtf8BS = TE.encodeUtf8 . renderCSIText

-- | Render a CSI directly to strict 'Text'.
--
-- You probably want to use 'renderCSI' instead.
-- This is just for testing.
renderCSIText :: CSI -> Text
renderCSIText = LT.toStrict . renderCSILazyText

-- | Render a CSI directly to lazy 'LT.Text'.
--
-- You probably want to use 'renderCSI' instead.
-- This is just for testing.
renderCSILazyText :: CSI -> LT.Text
renderCSILazyText = LTB.toLazyText . renderCSI

-- https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
renderCSI :: CSI -> Text.Builder
renderCSI =
  let csi ps c =
        mconcat
          [ LTB.singleton asciiEscape,
            LTB.singleton csiStart,
            renderCSIParams ps,
            LTB.singleton c
          ]
   in \case
        SGR sgrs -> csi (concatMap sgrToCSIParams sgrs) 'm'

-- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters
data SGR
  = Reset
  | SetItalic !Bool
  | SetUnderlining !Underlining
  | SetConsoleIntensity !ConsoleIntensity
  | SetColour !ColourIntensity !ConsoleLayer !TerminalColour
  | Set8BitColour !ConsoleLayer !Word8
  | Set24BitColour
      !ConsoleLayer
      !Word8 -- Red
      !Word8 -- Green
      !Word8 -- Blue
  deriving (Show, Eq, Generic)

instance Validity SGR

renderCSIParams :: [Word8] -> Text.Builder
renderCSIParams = mconcat . intersperse (LTB.singleton csiDelimiter) . map renderCSIParam

renderCSIParam :: Word8 -> Text.Builder
renderCSIParam = \case
  0 -> mempty
  w -> LTB.decimal w

sgrToCSIParams :: SGR -> [Word8]
sgrToCSIParams = \case
  Reset -> [] -- [0] would be fine too
  SetItalic b -> [if b then 3 else 23]
  SetUnderlining u ->
    [ case u of
        SingleUnderline -> 4
        DoubleUnderline -> 21
        NoUnderline -> 24
    ]
  SetConsoleIntensity ci ->
    [ case ci of
        BoldIntensity -> 1
        FaintIntensity -> 2
        NormalIntensity -> 22
    ]
  SetColour i l c ->
    [ case i of
        Dull -> case l of
          Foreground -> 30 + terminalColourSGRParameter c
          Background -> 40 + terminalColourSGRParameter c
        Bright -> case l of
          Foreground -> 90 + terminalColourSGRParameter c
          Background -> 100 + terminalColourSGRParameter c
    ]
  Set8BitColour l w ->
    [ case l of
        Foreground -> 38
        Background -> 48,
      5,
      w
    ]
  Set24BitColour l r g b ->
    [ case l of
        Foreground -> 38
        Background -> 48,
      2,
      r,
      g,
      b
    ]

-- | ANSI text underlining
data Underlining
  = SingleUnderline
  | DoubleUnderline
  | NoUnderline
  deriving (Show, Eq, Generic, Bounded, Enum)

instance Validity Underlining

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity
  = BoldIntensity
  | FaintIntensity
  | NormalIntensity
  deriving (Show, Eq, Generic, Bounded, Enum)

instance Validity ConsoleIntensity

-- | ANSI's standard colours come in two intensities
data ColourIntensity
  = Dull
  | Bright
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity ColourIntensity

-- | ANSI colours can be set on two different layers
data ConsoleLayer
  = Foreground
  | Background
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity ConsoleLayer

data TerminalColour
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity TerminalColour

terminalColourSGRParameter :: TerminalColour -> Word8
terminalColourSGRParameter = \case
  Black -> 0
  Red -> 1
  Green -> 2
  Yellow -> 3
  Blue -> 4
  Magenta -> 5
  Cyan -> 6
  White -> 7

terminalColourFromIndex :: Word8 -> Maybe TerminalColour
terminalColourFromIndex = \case
  0 -> Just Black
  1 -> Just Red
  2 -> Just Green
  3 -> Just Yellow
  4 -> Just Blue
  5 -> Just Magenta
  6 -> Just Cyan
  7 -> Just White
  _ -> Nothing
