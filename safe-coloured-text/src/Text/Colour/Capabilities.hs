{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour.Capabilities where

import GHC.Generics (Generic)

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
