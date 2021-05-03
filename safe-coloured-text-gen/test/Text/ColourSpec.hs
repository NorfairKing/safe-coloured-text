{-# LANGUAGE TypeApplications #-}

module Text.ColourSpec (spec) where

import Test.Syd
import Test.Syd.Validity
import Text.Colour
import Text.Colour.Code
import Text.Colour.Gen ()

spec :: Spec
spec = do
  genValidSpec @CSI
  genValidSpec @ColourIntensity
  genValidSpec @ConsoleIntensity
  genValidSpec @ConsoleLayer
  genValidSpec @SGR
  genValidSpec @TerminalColour
  genValidSpec @TerminalColour
  genValidSpec @Underlining
  genValidSpec @Colour
  genValidSpec @Chunk
