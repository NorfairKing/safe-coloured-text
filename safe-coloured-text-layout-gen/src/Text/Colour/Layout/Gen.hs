{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Colour.Layout.Gen where

import Data.GenValidity
import Text.Colour.Gen ()
import Text.Colour.Layout

instance GenValid Table where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TableBackground where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
