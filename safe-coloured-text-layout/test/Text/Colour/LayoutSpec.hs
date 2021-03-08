{-# LANGUAGE OverloadedStrings #-}

module Text.Colour.LayoutSpec (spec) where

import Test.Syd
import Text.Colour
import Text.Colour.Layout

spec :: Spec
spec = do
  let gf = ("test_resources/" ++)
  describe "layoutAsTable" $ do
    it "outputs this list the same as before" $ do
      pureGoldenByteStringFile
        (gf "list.dat")
        ( renderChunksBS
            With24BitColours
            ( layoutAsTable
                [ ["this"],
                  ["is"],
                  ["a"],
                  ["list"]
                ]
            )
        )
    it "outputs this two-column table the same as before" $ do
      pureGoldenByteStringFile
        (gf "two-columns.dat")
        ( renderChunksBS
            With24BitColours
            ( layoutAsTable
                [ ["this", "this"],
                  ["is", "is"],
                  ["a", "another"],
                  ["list", "list"]
                ]
            )
        )
    it "outputs this two-column table the same as before" $ do
      pureGoldenByteStringFile
        (gf "table-with-rows-of-unequal-length.dat")
        ( renderChunksBS
            With24BitColours
            ( layoutAsTable
                [ ["what", "the", ""],
                  ["is", "this", "", "-ing", ""],
                  ["I", "don't", "understand", "one", "", "of", "it"]
                ]
            )
        )
