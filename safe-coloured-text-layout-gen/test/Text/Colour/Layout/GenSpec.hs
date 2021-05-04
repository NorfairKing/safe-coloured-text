{-# LANGUAGE TypeApplications #-}

module Text.Colour.Layout.GenSpec (spec) where

import Test.Syd
import Test.Syd.Validity
import Text.Colour.Layout
import Text.Colour.Layout.Gen ()

spec :: Spec
spec = do
  genValidSpec @Table
  genValidSpec @TableBackground
