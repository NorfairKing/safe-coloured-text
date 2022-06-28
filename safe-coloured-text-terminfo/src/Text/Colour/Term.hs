module Text.Colour.Term where

import System.IO
import Text.Colour
import Text.Colour.Capabilities.FromEnv

-- | Print a list of chunks to 'stdout'.
--
-- This function will use 'getTerminalCapabilitiesHandle' on 'stdout'.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesFromEnv' first and then use 'putChunksUtf8With'.
putChunksUtf8 :: [Chunk] -> IO ()
putChunksUtf8 = hPutChunksUtf8 stdout

-- | Print a list of chunks to 'stdout' in an encoding according to the system's locale.
--
-- This function will use 'getTerminalCapabilitiesHandle' on 'stdout'.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesFromEnv' first and then use 'putChunksLocaleWith'.
putChunksLocale :: [Chunk] -> IO ()
putChunksLocale = hPutChunksLocale stdout

-- | Deprecated synonym of putChunksUtf8
putChunks :: [Chunk] -> IO ()
putChunks = putChunksUtf8
{-# DEPRECATED putChunks "Use putChunksLocale, or putChunksUtf8 if you must." #-}

-- | Print a list of chunks to the given 'Handle'
--
-- This function will use 'getTerminalCapabilitiesHandle' on the given handle.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesHandle' first and then use 'hPutChunksUtf8With'.
hPutChunksUtf8 :: Handle -> [Chunk] -> IO ()
hPutChunksUtf8 h cs = do
  tc <- getTerminalCapabilitiesFromHandle h
  hPutChunksUtf8With tc h cs

-- | Print a list of chunks to the given 'Handle' in an encoding according to the system's locale.
--
-- This function will use 'getTerminalCapabilitiesHandle' on the given handle.
-- If you intend to use this function more than once, it is more efficient to use 'getTerminalCapabilitiesHandle' first and then use 'hPutChunksLocaleWith'.
hPutChunksLocale :: Handle -> [Chunk] -> IO ()
hPutChunksLocale h cs = do
  tc <- getTerminalCapabilitiesFromHandle h
  hPutChunksLocaleWith tc h cs

-- | Deprecated synonym of 'hPutChunksUtf8'
hPutChunks :: Handle -> [Chunk] -> IO ()
hPutChunks = hPutChunksUtf8
{-# DEPRECATED hPutChunks "Use hPutChunksLocale, or hPutChunksUtf8 if you must." #-}
