module Text.Colour
  ( -- * Building chunks
    Chunk (..),
    chunk,
    fore,
    back,

    -- ** Styling
    bold,
    faint,
    italic,
    underline,

    -- ** Colours
    Colour (..),
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    brightBlack,
    brightRed,
    brightGreen,
    brightYellow,
    brightBlue,
    brightMagenta,
    brightCyan,
    brightWhite,
    color256,
    colour256,

    -- * IO
    TerminalCapabilities (..),
    getTerminalCapabilitiesFromEnv,
    getTerminalCapabilitiesFromHandle,

    -- ** Rendering chunks to bytestring builders
    renderChunks,
    renderChunk,

    -- ** Rendering chunks to strict bytestring
    renderChunksBS,
    renderChunkBS,

    -- ** Outputting chunks directly
    putChunks,
    hPutChunks,
    putChunksWith,
    hPutChunksWith,
  )
where

import qualified Data.ByteString.Builder as SBB
import System.IO
import Text.Colour.Capabilities
import Text.Colour.Chunk

putChunks :: [Chunk] -> IO ()
putChunks = hPutChunks stdout

hPutChunks :: Handle -> [Chunk] -> IO ()
hPutChunks h cs = do
  tc <- getTerminalCapabilitiesFromHandle h
  hPutChunksWith tc h cs

putChunksWith :: TerminalCapabilities -> [Chunk] -> IO ()
putChunksWith tc = hPutChunksWith tc stdout

hPutChunksWith :: TerminalCapabilities -> Handle -> [Chunk] -> IO ()
hPutChunksWith tc h cs = SBB.hPutBuilder h $ renderChunks tc cs
