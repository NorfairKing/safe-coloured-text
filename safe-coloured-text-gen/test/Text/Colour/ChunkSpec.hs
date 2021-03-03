{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Colour.ChunkSpec (spec) where

import Control.Monad
import Data.Char as Char
import Data.List
import qualified Data.Text as T
import Test.Syd
import Text.Colour.Capabilities
import Text.Colour.Chunk
import Text.Colour.Code

spec :: Spec
spec = do
  let gf = ("test_resources/chunk/" ++)
  describe "renderChunk" $ do
    it "outputs a plain chunk the same as before" $
      pureGoldenByteStringFile (gf "plain.dat") (renderChunkBS With256Colours (chunk "Hello world"))
    let chunks string = do
          let colour = do
                terminalColour <- [minBound .. maxBound]
                intensity <- [minBound .. maxBound]
                pure $ Colour intensity terminalColour
          let mColour = Nothing : map Just colour
          chunkItalic <- Nothing : map Just [minBound .. maxBound]
          chunkConsoleIntensity <- Nothing : map Just [minBound .. maxBound]
          chunkUnderlining <- Nothing : map Just [minBound .. maxBound]
          chunkForeground <- mColour
          chunkBackground <- mColour
          let chunkText = T.pack string
          let colourName (Colour i tc) = unwords [show i, show tc]
              mColourName :: Maybe Colour -> String
              mColourName Nothing = "no"
              mColourName (Just c) = "a " <> colourName c
              italicName i = if i then "non-italic" else "italic"
              consoleIntensityName :: ConsoleIntensity -> String
              consoleIntensityName = \case
                BoldIntensity -> "bold"
                FaintIntensity -> "faint"
                NormalIntensity -> "non-bold"
              underliningName :: Underlining -> String
              underliningName = \case
                SingleUnderline -> "underline"
                DoubleUnderline -> "double underline"
                NoUnderline -> "no underline"
              name =
                unwords $
                  filter
                    (not . null)
                    [ maybe "" italicName chunkItalic,
                      maybe "" consoleIntensityName chunkConsoleIntensity,
                      maybe "" underliningName chunkUnderlining,
                      string,
                      "with",
                      mColourName chunkForeground,
                      "foreground on",
                      mColourName chunkBackground,
                      "background"
                    ]
              colourPath (Colour i tc) = map Char.toLower $ intercalate "-" [show i, show tc]
              mColourPath :: Maybe Colour -> FilePath
              mColourPath Nothing = "no"
              mColourPath (Just c) = colourPath c
              italicPath i = if i then "non-italic" else "italic"
              consoleIntensityPath :: ConsoleIntensity -> FilePath
              consoleIntensityPath = \case
                BoldIntensity -> "bold"
                FaintIntensity -> "faint"
                NormalIntensity -> "non-bold"
              underliningPath :: Underlining -> FilePath
              underliningPath = \case
                SingleUnderline -> "underline"
                DoubleUnderline -> "double-underline"
                NoUnderline -> "no-underline"
              path =
                intercalate
                  "-"
                  ( filter
                      (not . null)
                      [ maybe "" italicPath chunkItalic,
                        maybe "" consoleIntensityPath chunkConsoleIntensity,
                        maybe "" underliningPath chunkUnderlining,
                        mColourPath chunkForeground,
                        "fg",
                        mColourPath chunkBackground,
                        "bg"
                      ]
                  )
                  <> ".dat"
          pure (name, path, Chunk {..})

    forM_ (chunks "Hello world") $ \(name, path, c) ->
      it (unwords ["outputs a", name, "the same way as before"]) $
        pureGoldenByteStringFile (gf path) (renderChunkBS With256Colours c)
