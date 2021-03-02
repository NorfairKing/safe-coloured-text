{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour.Capabilities where

import Control.Exception
import GHC.Generics (Generic)
import qualified System.Console.Terminfo as Terminfo
import System.IO

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
