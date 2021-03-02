{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour where

import Control.Exception
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as SBB
import GHC.Generics (Generic)
import qualified System.Console.Terminfo as Terminfo
import System.IO
import Text.Colour.Chunk

data TerminalCapabilities
  = NoColour
  | Colours
  deriving (Show, Eq, Generic)

getTerminalCapabilitiesFromEnv :: IO TerminalCapabilities
getTerminalCapabilitiesFromEnv = do
  mTerm <- (Just <$> Terminfo.setupTermFromEnv) `catch` (\(_ :: Terminfo.SetupTermError) -> pure Nothing)
  pure $ case mTerm of
    Nothing -> NoColour
    Just term -> case Terminfo.getCapability term (Terminfo.tiGetNum "colors") of
      Nothing -> NoColour
      Just c
        | c >= 8 -> Colours
        | otherwise -> NoColour

getTerminalCapabilitiesFromHandle :: Handle -> IO TerminalCapabilities
getTerminalCapabilitiesFromHandle h = do
  isTerm <- hIsTerminalDevice h
  if isTerm
    then getTerminalCapabilitiesFromEnv
    else pure NoColour

renderChunks :: Foldable f => TerminalCapabilities -> f Chunk -> Builder
renderChunks tc = foldMap (renderChunk tc)

renderChunk :: TerminalCapabilities -> Chunk -> Builder
renderChunk = \case
  Colours -> renderChunkWithColour
  NoColour -> renderChunkWithoutColour

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
