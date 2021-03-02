{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Text.Colour.Code where

import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Char8 as SB8
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
  = SGR [SGR]

-- | Render an CSI directly to bytestring.
-- You probably want to use 'renderCSI' instead.
-- This is just for testing.
renderCSIBS :: CSI -> ByteString
renderCSIBS = LB.toStrict . SBB.toLazyByteString . renderCSI

-- https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
renderCSI :: CSI -> Builder
renderCSI = \case
  SGR sgrs ->
    mconcat
      [ SBB.word8 asciiEscape,
        SBB.word8 csiStart,
        csiParamsToWords $ concatMap sgrToCSIParams sgrs,
        SBB.word8 (SBI.c2w 'm')
      ]

-- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters
data SGR
  = Reset
  | SetColour ColorIntensity ConsoleLayer TerminalColour
  deriving (Show, Eq, Generic)

csiParamsToWords :: [Word8] -> Builder
csiParamsToWords = mconcat . intersperse (SBB.word8 csiDelimiter) . map csiParamToWords

-- Is supposed to turn:
--
-- 0  -> "0"   ASCII encoded
-- 10 -> "10"  ASCII encoded
csiParamToWords :: Word8 -> Builder
csiParamToWords = SBB.word8Dec

sgrToCSIParams :: SGR -> [Word8]
sgrToCSIParams = \case
  Reset -> [] -- [0] would be fine too
  SetColour i l c ->
    [ case i of
        Dull -> case l of
          Foreground -> 30 + terminalColourSGRParameter c
          Background -> 40 + terminalColourSGRParameter c
        Bright -> case l of
          Foreground -> 90 + terminalColourSGRParameter c
          Background -> 100 + terminalColourSGRParameter c
    ]

-- | ANSI's standard colors come in two intensities
data ColorIntensity
  = Dull
  | Bright
  deriving (Show, Eq, Generic, Enum, Bounded)

-- | ANSI colors can be set on two different layers
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
