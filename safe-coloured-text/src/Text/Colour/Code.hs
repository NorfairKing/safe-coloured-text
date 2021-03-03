{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Text.Colour.Code where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Internal as SBI
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Word
import GHC.Generics (Generic)

-- https://en.wikipedia.org/wiki/Escape_character#ASCII_escape_character
asciiEscape :: Word8
asciiEscape = SBI.c2w '\ESC'

csiStart :: Word8
csiStart = SBI.c2w '['

csiDelimiter :: Word8
csiDelimiter = SBI.c2w ';'

data CSI
  = SGR ![SGR]
  deriving (Show, Eq, Generic)

-- | Render a CSI directly to bytestring.
-- You probably want to use 'renderCSI' instead.
-- This is just for testing.
renderCSIBS :: CSI -> ByteString
renderCSIBS = LB.toStrict . SBB.toLazyByteString . renderCSI

-- https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
renderCSI :: CSI -> Builder
renderCSI =
  let csi ps c =
        mconcat
          [ SBB.word8 asciiEscape,
            SBB.word8 csiStart,
            csiParamsToWords ps,
            SBB.word8 c
          ]
   in \case
        SGR sgrs -> csi (concatMap sgrToCSIParams sgrs) (SBI.c2w 'm')

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

csiParamsToWords :: [Word8] -> Builder
csiParamsToWords = mconcat . intersperse (SBB.word8 csiDelimiter) . map SBB.word8Dec

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

-- | ANSI general console intensity: usually treated as setting the font style
-- (e.g. 'BoldIntensity' causes text to be bold)
data ConsoleIntensity
  = BoldIntensity
  | FaintIntensity
  | NormalIntensity
  deriving (Show, Eq, Generic, Bounded, Enum)

-- | ANSI's standard colours come in two intensities
data ColourIntensity
  = Dull
  | Bright
  deriving (Show, Eq, Generic, Enum, Bounded)

-- | ANSI colours can be set on two different layers
data ConsoleLayer
  = Foreground
  | Background
  deriving (Show, Eq, Generic, Enum, Bounded)

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
