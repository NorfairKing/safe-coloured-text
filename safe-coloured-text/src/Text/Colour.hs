{-# LANGUAGE OverloadedStrings #-}

-- | Safe Coloured Text
--
-- This module is responsible for defining, building, and rendering coloured text.
--
-- The text to be coloured is 'Text', but the rendered text, while technically still (probably) valid Utf8, will be a 'ByteString' builder.
module Text.Colour
  ( -- * Building chunks
    chunk,
    Chunk (..),

    -- ** Styling

    -- *** Setting colour

    --
    -- These will only be rendered if the given 'TerminalCapabilities' supports them.
    fore,
    back,

    -- *** Setting non-colour attributes

    --
    -- These will be rendered for any 'TerminalCapabilities'
    bold,
    faint,
    italic,
    underline,
    doubleUnderline,
    noUnderline,
    slowBlinking,
    rapidBlinking,
    noBlinking,

    -- ** Colours
    Colour (..),

    -- *** 8-colour

    -- **** Dull
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,

    -- **** Bright
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,

    -- *** 8-bit
    color256,
    colour256,

    -- *** 24-bit
    colorRGB,
    colourRGB,

    -- * Rendering

    -- ** Composing chunks
    unlinesChunks,
    unwordsChunks,

    -- ** Rendering chunks to strict bytestring in UTF8
    renderChunksUtf8BS,
    renderChunkUtf8BS,

    -- ** Rendering chunks to lazy bytestring builders in UTF8
    renderChunksUtf8BSBuilder,
    renderChunkUtf8BSBuilder,

    -- ** Rendering chunks to strict Text
    renderChunksText,
    renderChunkText,

    -- ** Rendering chunks to lazy text
    renderChunksLazyText,
    renderChunkLazyText,

    -- ** Rendering chunks to lazy text builder
    renderChunksBuilder,
    renderChunkBuilder,

    -- * IO
    TerminalCapabilities (..),

    -- ** Outputting chunks directly
    putChunksUtf8With,
    putChunksLocaleWith,
    hPutChunksUtf8With,
    hPutChunksLocaleWith,
  )
where

import qualified Data.ByteString.Builder as SBB
import Data.List (intercalate)
import qualified Data.Text.IO as TIO
import System.IO
import Text.Colour.Capabilities
import Text.Colour.Chunk

-- | Print a list of chunks to stdout with given 'TerminalCapabilities'.
putChunksUtf8With :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksUtf8With tc = hPutChunksUtf8With tc stdout

-- | Print a list of chunks to stdout with given 'TerminalCapabilities' in an encoding according to the system's locale.
putChunksLocaleWith :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksLocaleWith tc = hPutChunksLocaleWith tc stdout

-- | Print a list of chunks to the given 'Handle' with given 'TerminalCapabilities'.
hPutChunksUtf8With :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksUtf8With tc h cs = SBB.hPutBuilder h $ renderChunksUtf8BSBuilder tc cs

-- | Print a list of chunks to the given 'Handle' with given 'TerminalCapabilities' in an encoding according to the system's locale.
hPutChunksLocaleWith :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksLocaleWith tc h cs = TIO.hPutStr h $ renderChunksText tc cs

-- | Render lines of chunks.
--
-- This puts newlines ("\n") at the end of every list of chunks.
unlinesChunks :: [[Chunk]] -> [Chunk]
unlinesChunks = concatMap (<> [chunk "\n"])

-- | Render lines of chunks.
--
-- This puts newlines (" ") inbetween the list of chunks.
unwordsChunks :: [[Chunk]] -> [Chunk]
unwordsChunks = intercalate [" "]
