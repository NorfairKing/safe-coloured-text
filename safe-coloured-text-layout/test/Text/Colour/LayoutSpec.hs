{-# LANGUAGE OverloadedStrings #-}

module Text.Colour.LayoutSpec (spec) where

import qualified Data.Text as T
import Test.Syd
import Text.Colour
import Text.Colour.Layout

spec :: Spec
spec = do
  let gf = ("test_resources/" ++)
  describe "layoutAsTable" $ do
    it "outputs this list the same as before" $ do
      pureGoldenTextFile
        (gf "list.dat")
        ( renderChunksText
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
      pureGoldenTextFile
        (gf "two-columns.dat")
        ( renderChunksText
            With24BitColours
            ( layoutAsTable
                [ ["this", "this"],
                  ["is", "is"],
                  ["a", "another"],
                  ["list", "list"]
                ]
            )
        )
    it "outputs this weird two-column table the same as before" $ do
      pureGoldenTextFile
        (gf "table-with-rows-of-unequal-length.dat")
        ( renderChunksText
            With24BitColours
            ( layoutAsTable
                [ ["what", "the", ""],
                  ["is", "this", "", "-ing", ""],
                  ["I", "don't", "understand", "one", "", "of", "it"]
                ]
            )
        )
    it "outputs this two-column table with a custom column separator the same as before" $ do
      pureGoldenTextFile
        (gf "custom-column-separator.dat")
        ( renderChunksText
            With24BitColours
            ( renderTable $
                ( table $ [[chunk (T.pack (show (x + y))) | x <- [0 :: Int .. 9]] | y <- [0 :: Int .. 9]]
                )
                  { tableColumnSeparator = "@"
                  }
            )
        )
    it "outputs this table with a background colour the same as before" $ do
      pureGoldenTextFile
        (gf "background.dat")
        ( renderChunksText
            With24BitColours
            ( renderTable $
                ( table $ [[fore red $ chunk (T.pack (show (x ^ y))) | x <- [0 :: Int .. 4]] | y <- [0 :: Int .. 4]]
                )
                  { tableBackground = Just (SingleColour black)
                  }
            )
        )
    it "outputs this table with a bicoloured background the same as before" $ do
      pureGoldenTextFile
        (gf "bicolour-background.dat")
        ( renderChunksText
            With24BitColours
            ( renderTable $
                ( table $ [[fore red $ chunk (T.pack (show (x ^ y))) | x <- [0 :: Int .. 4]] | y <- [0 :: Int .. 4]]
                )
                  { tableBackground = Just (Bicolour (Just black) (Just brightBlack))
                  }
            )
        )
