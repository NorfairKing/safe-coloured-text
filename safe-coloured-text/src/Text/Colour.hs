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

    -- ** Decrecated rendering chunks to strict bytestring in UTF8
    renderChunksBS,
    renderChunkBS,

    -- ** Deprecated rendering chunks to lazy bytestring builders in UTF8
    renderChunks,
    renderChunk,

    -- * IO
    TerminalCapabilities (..),

    -- ** Outputting chunks directly
    putChunksUtf8With,
    putChunksLocaleWith,
    hPutChunksUtf8With,
    hPutChunksLocaleWith,
    putChunksWith,
    hPutChunksWith,
  )
where

import qualified Data.ByteString.Builder as SBB
import qualified Data.Text.IO as TIO
import System.IO
import Text.Colour.Capabilities
import Text.Colour.Chunk

-- | Print a list of chunks to stdout with given 'TerminalCapabilities'.
putChunksUtf8With :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksUtf8With tc = hPutChunksUtf8With tc stdout

-- | Print a list of chunks to stdout with given 'TerminalCapabilities' in an encoding according to the system's locale.
putChunksLocaleWith :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksLocaleWith tc = hPutChunksWith tc stdout

-- | Deprecated synonym of 'putChunksUtf8With'
putChunksWith :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksWith = putChunksUtf8With
{-# DEPRECATED putChunksWith "Use putChunksLocaleWith, or putChunksUtf8With if you must." #-}

-- | Print a list of chunks to the given 'Handle' with given 'TerminalCapabilities'.
hPutChunksUtf8With :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksUtf8With tc h cs = SBB.hPutBuilder h $ renderChunksUtf8BSBuilder tc cs

-- | Print a list of chunks to the given 'Handle' with given 'TerminalCapabilities' in an encoding according to the system's locale.
hPutChunksLocaleWith :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksLocaleWith tc h cs = TIO.hPutStr h $ renderChunksText tc cs

-- | Deprecated synonym of 'hPutChunksUtf8With'
hPutChunksWith :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksWith = hPutChunksUtf8With
{-# DEPRECATED hPutChunksWith "Use hPutChunksLocaleWith, or hPutChunksUtf8With if you must." #-}
