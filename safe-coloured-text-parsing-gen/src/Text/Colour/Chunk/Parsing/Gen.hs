{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Colour.Chunk.Parsing.Gen where

import Data.GenValidity
import Text.Colour.Chunk.Parsing
import Text.Colour.Gen ()

instance GenValid OscCommand where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AnsiToken where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
