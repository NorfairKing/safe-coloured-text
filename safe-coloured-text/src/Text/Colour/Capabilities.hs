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
  = -- | No colours
    WithoutColours
  | -- | Only 8 colours
    With8Colours
  | -- | Only 8-bit colours
    With8BitColours
  | -- | All 24-bit colours
    With24BitColours
  deriving (Show, Eq, Ord, Generic)

-- | Try to detect how many colours the terminal can handle.
--
-- This is based on the @colors@ capability of the terminfo detected based on the @TERM@ environment variable.
-- If the terminal can handle 8-bit colours and also has the @COLORTERM@ environment variable set to @24bit@ or @truecolor@, then this function will return 'With24BitColours'.
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

-- | Try to detect how many colours a given handle can handle.
--
-- This function does the same as 'getTerminalCapabilitiesFromEnv' but returns 'WithoutColours' is not a terminal device.
getTerminalCapabilitiesFromHandle :: Handle -> IO TerminalCapabilities
getTerminalCapabilitiesFromHandle h = do
  isTerm <- hIsTerminalDevice h
  if isTerm
    then getTerminalCapabilitiesFromEnv
    else pure WithoutColours
