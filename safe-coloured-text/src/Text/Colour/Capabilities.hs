{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour.Capabilities where

import Control.Exception
import GHC.Generics (Generic)
import qualified System.Console.Terminfo as Terminfo
import System.IO

data TerminalCapabilities
  = WithoutColours
  | With8Colours
  | With256Colours
  deriving (Show, Eq, Generic)

getTerminalCapabilitiesFromEnv :: IO TerminalCapabilities
getTerminalCapabilitiesFromEnv = do
  mTerm <- (Just <$> Terminfo.setupTermFromEnv) `catch` (\(_ :: Terminfo.SetupTermError) -> pure Nothing)
  pure $ case mTerm of
    Nothing -> WithoutColours
    Just term -> case Terminfo.getCapability term (Terminfo.tiGetNum "colors") of
      Nothing -> WithoutColours
      Just c
        | c >= 256 -> With256Colours
        | c >= 8 -> With8Colours
        | otherwise -> WithoutColours

getTerminalCapabilitiesFromHandle :: Handle -> IO TerminalCapabilities
getTerminalCapabilitiesFromHandle h = do
  isTerm <- hIsTerminalDevice h
  if isTerm
    then getTerminalCapabilitiesFromEnv
    else pure WithoutColours
