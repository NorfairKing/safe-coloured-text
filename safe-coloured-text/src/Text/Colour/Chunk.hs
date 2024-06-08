{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

module Text.Colour.Chunk where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as ByteString
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Text.Colour.Capabilities
import Text.Colour.Code

data Chunk = Chunk
  { chunkText :: !Text,
    chunkItalic :: !(Maybe Bool),
    chunkConsoleIntensity :: !(Maybe ConsoleIntensity),
    chunkUnderlining :: !(Maybe Underlining),
    chunkBlinking :: !(Maybe Blinking),
    chunkForeground :: !(Maybe Colour),
    chunkBackground :: !(Maybe Colour)
  }
  deriving (Show, Eq, Generic)

instance Validity Chunk

instance IsString Chunk where
  fromString = chunk . fromString

-- TODO This is not correct because text-width is correct but it's a
-- good place to put this so we can fix it later and it'll get fixed
-- everywhere.
chunkWidth :: Chunk -> Int
chunkWidth = T.length . chunkText

plainChunk :: TerminalCapabilities -> Chunk -> Bool
plainChunk tc Chunk {..} =
  let Chunk _ _ _ _ _ _ _ = undefined
   in and
        [ isNothing chunkItalic,
          isNothing chunkConsoleIntensity,
          isNothing chunkUnderlining,
          isNothing chunkBlinking,
          maybe True (plainColour tc) chunkForeground,
          maybe True (plainColour tc) chunkBackground
        ]

plainColour :: TerminalCapabilities -> Colour -> Bool
plainColour tc = \case
  Colour8 {} -> tc < With8Colours
  Colour8Bit {} -> tc < With8BitColours
  Colour24Bit {} -> tc < With24BitColours

-- | Render chunks directly to a UTF8-encoded 'Bytestring'.
renderChunksUtf8BS :: (Foldable f) => TerminalCapabilities -> f Chunk -> ByteString
renderChunksUtf8BS tc = TE.encodeUtf8 . renderChunksText tc

-- | Render chunks to a UTF8-encoded 'ByteString' 'Bytestring.Builder'.
renderChunksUtf8BSBuilder :: (Foldable f) => TerminalCapabilities -> f Chunk -> ByteString.Builder
renderChunksUtf8BSBuilder tc = foldMap (renderChunkUtf8BSBuilder tc)

-- | Render chunks directly to strict 'Text'.
renderChunksText :: (Foldable f) => TerminalCapabilities -> f Chunk -> Text
renderChunksText tc = LT.toStrict . renderChunksLazyText tc

-- | Render chunks directly to lazy 'LT.Text'.
renderChunksLazyText :: (Foldable f) => TerminalCapabilities -> f Chunk -> LT.Text
renderChunksLazyText tc = LTB.toLazyText . renderChunksBuilder tc

-- | Render chunks to a lazy 'LT.Text' 'Text.Builder'
renderChunksBuilder :: (Foldable f) => TerminalCapabilities -> f Chunk -> Text.Builder
renderChunksBuilder tc = foldMap (renderChunkBuilder tc)

-- | Render a chunk directly to a UTF8-encoded 'Bytestring'.
renderChunkUtf8BS :: TerminalCapabilities -> Chunk -> ByteString
renderChunkUtf8BS tc = TE.encodeUtf8 . renderChunkText tc

-- | Render a chunk directly to a UTF8-encoded 'Bytestring' 'ByteString.Builder'.
renderChunkUtf8BSBuilder :: TerminalCapabilities -> Chunk -> ByteString.Builder
renderChunkUtf8BSBuilder tc = LTE.encodeUtf8Builder . renderChunkLazyText tc

-- | Render a chunk directly to strict 'Text'.
renderChunkText :: TerminalCapabilities -> Chunk -> Text
renderChunkText tc = LT.toStrict . renderChunkLazyText tc

-- | Render a chunk directly to strict 'LT.Text'.
renderChunkLazyText :: TerminalCapabilities -> Chunk -> LT.Text
renderChunkLazyText tc = LTB.toLazyText . renderChunkBuilder tc

-- | Render a chunk to a lazy 'LT.Text' 'Text.Builder'
renderChunkBuilder :: TerminalCapabilities -> Chunk -> Text.Builder
renderChunkBuilder tc c@Chunk {..} =
  if plainChunk tc c
    then LTB.fromText chunkText
    else
      mconcat
        [ renderCSI (SGR (chunkSGR tc c)),
          LTB.fromText chunkText,
          renderCSI (SGR [Reset])
        ]

chunkSGR :: TerminalCapabilities -> Chunk -> [SGR]
chunkSGR tc Chunk {..} =
  catMaybes
    [ SetItalic <$> chunkItalic,
      SetUnderlining <$> chunkUnderlining,
      SetConsoleIntensity <$> chunkConsoleIntensity,
      chunkForeground >>= colourSGR tc Foreground,
      chunkBackground >>= colourSGR tc Background
    ]

-- | Turn a text into a plain chunk, without any styling
chunk :: Text -> Chunk
chunk t =
  Chunk
    { chunkText = t,
      chunkItalic = Nothing,
      chunkConsoleIntensity = Nothing,
      chunkUnderlining = Nothing,
      chunkBlinking = Nothing,
      chunkForeground = Nothing,
      chunkBackground = Nothing
    }

fore :: Colour -> Chunk -> Chunk
fore col chu = chu {chunkForeground = Just col}

back :: Colour -> Chunk -> Chunk
back col chu = chu {chunkBackground = Just col}

bold :: Chunk -> Chunk
bold chu = chu {chunkConsoleIntensity = Just BoldIntensity}

faint :: Chunk -> Chunk
faint chu = chu {chunkConsoleIntensity = Just FaintIntensity}

italic :: Chunk -> Chunk
italic chu = chu {chunkItalic = Just True}

underline :: Chunk -> Chunk
underline chu = chu {chunkUnderlining = Just SingleUnderline}

doubleUnderline :: Chunk -> Chunk
doubleUnderline chu = chu {chunkUnderlining = Just DoubleUnderline}

noUnderline :: Chunk -> Chunk
noUnderline chu = chu {chunkUnderlining = Just NoUnderline}

slowBlinking :: Chunk -> Chunk
slowBlinking chu = chu {chunkBlinking = Just SlowBlinking}

rapidBlinking :: Chunk -> Chunk
rapidBlinking chu = chu {chunkBlinking = Just RapidBlinking}

noBlinking :: Chunk -> Chunk
noBlinking chu = chu {chunkBlinking = Just NoBlinking}

-- TODO consider allowing an 8-colour alternative to a given 256-colour
data Colour
  = Colour8 !ColourIntensity !TerminalColour
  | Colour8Bit !Word8 -- The 8-bit colour
  | Colour24Bit !Word8 !Word8 !Word8
  deriving (Show, Eq, Generic)

instance Validity Colour

colourSGR :: TerminalCapabilities -> ConsoleLayer -> Colour -> Maybe SGR
colourSGR tc layer =
  let cap tc' sgr = if tc >= tc' then Just sgr else Nothing
   in \case
        Colour8 intensity terminalColour -> cap With8Colours $ SetColour intensity layer terminalColour
        Colour8Bit w -> cap With8BitColours $ Set8BitColour layer w
        Colour24Bit r g b -> cap With24BitColours $ Set24BitColour layer r g b

black :: Colour
black = Colour8 Dull Black

red :: Colour
red = Colour8 Dull Red

green :: Colour
green = Colour8 Dull Green

yellow :: Colour
yellow = Colour8 Dull Yellow

blue :: Colour
blue = Colour8 Dull Blue

magenta :: Colour
magenta = Colour8 Dull Magenta

cyan :: Colour
cyan = Colour8 Dull Cyan

white :: Colour
white = Colour8 Dull White

brightBlack :: Colour
brightBlack = Colour8 Bright Black

brightRed :: Colour
brightRed = Colour8 Bright Red

brightGreen :: Colour
brightGreen = Colour8 Bright Green

brightYellow :: Colour
brightYellow = Colour8 Bright Yellow

brightBlue :: Colour
brightBlue = Colour8 Bright Blue

brightMagenta :: Colour
brightMagenta = Colour8 Bright Magenta

brightCyan :: Colour
brightCyan = Colour8 Bright Cyan

brightWhite :: Colour
brightWhite = Colour8 Bright White

-- | Bulid an 8-bit RGB Colour
--
-- This will not be rendered unless 'With8BitColours' is used.
colour256 :: Word8 -> Colour
colour256 = Colour8Bit

-- | Alias for 'colour256', bloody americans...
color256 :: Word8 -> Colour
color256 = colour256

-- | Bulid a 24-bit RGB Colour
--
-- This will not be rendered unless 'With24BitColours' is used.
colourRGB :: Word8 -> Word8 -> Word8 -> Colour
colourRGB = Colour24Bit

-- | Alias for 'colourRGB', bloody americans...
colorRGB :: Word8 -> Word8 -> Word8 -> Colour
colorRGB = Colour24Bit
