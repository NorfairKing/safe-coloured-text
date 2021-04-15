module Text.Colour.Term where

import System.IO
import Text.Colour
import Text.Colour.Capabilities.FromEnv

-- | Print a list of chunks to 'stdout'.
--
-- This function will use 'getTerminalCapabilitiesHandle' on 'stdout'.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesFromEnv' first and then use 'putChunksWith'.
putChunks :: [Chunk] -> IO ()
putChunks = hPutChunks stdout

-- | Print a list of chunks to the given 'Handle'
--
-- This function will use 'getTerminalCapabilitiesHandle' on the given handle.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesHandle' first and then use 'hPutChunksWith'.
hPutChunks :: Handle -> [Chunk] -> IO ()
hPutChunks h cs = do
  tc <- getTerminalCapabilitiesFromHandle h
  hPutChunksWith tc h cs
