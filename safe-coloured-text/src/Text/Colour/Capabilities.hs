{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour.Capabilities where

import Control.Exception
import GHC.Generics (Generic)
import qualified System.Console.Terminfo as Terminfo
import System.Environment (lookupEnv)
import System.IO

-- Note that the order of these constructors matters!
data TerminalCapabilities
  = WithoutColours
  | With8Colours
  | With8BitColours
  | With24BitColours
  deriving (Show, Eq, Ord, Generic)

getTerminalCapabilitiesFromEnv :: IO TerminalCapabilities
getTerminalCapabilitiesFromEnv = do
  mTerm <- (Just <$> Terminfo.setupTermFromEnv) `catch` (\(_ :: Terminfo.SetupTermError) -> pure Nothing)
  case mTerm of
    Nothing -> pure WithoutColours
    Just term -> do
      -- To support 24-bit colour:
      -- https://unix.stackexchange.com/questions/450365/check-if-terminal-supports-24-bit-true-color
      mct <- lookupEnv "COLORTERM"
      pure $ case mct of
        Just "truecolor" -> With24BitColours
        Just "24bit" -> With24BitColours
        _ ->
          case Terminfo.getCapability term (Terminfo.tiGetNum "colors") of
            Nothing -> WithoutColours
            Just c
              | c > 256 -> With24BitColours
              | c >= 256 -> With8BitColours
              | c >= 8 -> With8Colours
              | otherwise -> WithoutColours

getTerminalCapabilitiesFromHandle :: Handle -> IO TerminalCapabilities
getTerminalCapabilitiesFromHandle h = do
  isTerm <- hIsTerminalDevice h
  if isTerm
    then getTerminalCapabilitiesFromEnv
    else pure WithoutColours
