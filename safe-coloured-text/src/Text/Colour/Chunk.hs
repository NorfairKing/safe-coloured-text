{-# LANGUAGE RecordWildCards #-}

module Text.Colour.Chunk where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Text.Colour.Code

data Chunk = Chunk
  { chunkText :: !Text,
    chunkItalic :: !(Maybe Bool),
    chunkConsoleIntensity :: !(Maybe ConsoleIntensity),
    chunkUnderlining :: !(Maybe Underlining),
    chunkForeground :: !(Maybe Colour),
    chunkBackground :: !(Maybe Colour)
  }

-- | Render a chunk directly to bytestring.
-- You probably want to use 'renderChunk' instead.
-- This is just for testing.
renderChunkBS :: Chunk -> ByteString
renderChunkBS = LB.toStrict . SBB.toLazyByteString . renderChunk

renderChunk :: Chunk -> Builder
renderChunk c@Chunk {..} =
  mconcat
    [ renderCSI (SGR $ chunkSGRs c),
      SBB.byteString (TE.encodeUtf8 chunkText),
      renderCSI (SGR [Reset])
    ]

chunkSGRs :: Chunk -> [SGR]
chunkSGRs Chunk {..} =
  catMaybes
    [ SetItalic <$> chunkItalic,
      SetUnderlining <$> chunkUnderlining,
      SetConsoleIntensity <$> chunkConsoleIntensity,
      colourSGR Foreground <$> chunkForeground,
      colourSGR Background <$> chunkBackground
    ]

chunk :: Text -> Chunk
chunk t =
  Chunk
    { chunkText = t,
      chunkItalic = Nothing,
      chunkConsoleIntensity = Nothing,
      chunkUnderlining = Nothing,
      chunkForeground = Nothing,
      chunkBackground = Nothing
    }

fore :: Colour -> Chunk -> Chunk
fore col chu = chu {chunkForeground = Just col}

back :: Colour -> Chunk -> Chunk
back col chu = chu {chunkBackground = Just col}

data Colour = Colour
  { colourIntensity :: ColourIntensity,
    colourValue :: TerminalColour
  }

colourSGR :: ConsoleLayer -> Colour -> SGR
colourSGR layer Colour {..} = SetColour colourIntensity layer colourValue

black :: Colour
black = Colour Dull Black

red :: Colour
red = Colour Dull Red

green :: Colour
green = Colour Dull Green

yellow :: Colour
yellow = Colour Dull Yellow

blue :: Colour
blue = Colour Dull Blue

magenta :: Colour
magenta = Colour Dull Magenta

cyan :: Colour
cyan = Colour Dull Cyan

white :: Colour
white = Colour Dull White

brightBlack :: Colour
brightBlack = Colour Bright Black

brightRed :: Colour
brightRed = Colour Bright Red

brightGreen :: Colour
brightGreen = Colour Bright Green

brightYellow :: Colour
brightYellow = Colour Bright Yellow

brightBlue :: Colour
brightBlue = Colour Bright Blue

brightMagenta :: Colour
brightMagenta = Colour Bright Magenta

brightCyan :: Colour
brightCyan = Colour Bright Cyan

brightWhite :: Colour
brightWhite = Colour Bright White
