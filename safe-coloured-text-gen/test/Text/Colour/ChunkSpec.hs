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
    describe "8 colours" $ do
      let gf8 = ("test_resources/chunk/8/" ++)
      let chunks string = do
            let colour = do
                  terminalColour <- [minBound .. maxBound]
                  intensity <- [minBound .. maxBound]
                  pure $ Colour8 intensity terminalColour
            let mColour = Nothing : map Just colour
            chunkItalic <- Nothing : map Just [minBound .. maxBound]
            chunkConsoleIntensity <- Nothing : map Just [minBound .. maxBound]
            chunkUnderlining <- Nothing : map Just [minBound .. maxBound]
            chunkForeground <- mColour
            chunkBackground <- mColour
            let chunkText = T.pack string
            let italicName i = if i then "non-italic" else "italic"
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
          pureGoldenByteStringFile (gf8 path) (renderChunkBS With256Colours c)
    describe "256 colours" $ do
      let gf256 = ("test_resources/chunk/256/" ++)
      let chunks string = do
            let colour = do
                  w <- (* 16) <$> [0 .. 15] -- Just a few colours, otherwise we end up with 65K files.
                  pure $ Colour256 w
            let mColour = Nothing : map Just colour
            let chunkItalic = Nothing
            let chunkConsoleIntensity = Nothing
            let chunkUnderlining = Nothing
            chunkForeground <- mColour
            chunkBackground <- mColour
            let chunkText = T.pack string
            let name =
                  unwords $
                    [ mColourName chunkForeground,
                      "foreground on",
                      mColourName chunkBackground,
                      "background"
                    ]
                path =
                  intercalate
                    "-"
                    ( [ mColourPath chunkForeground,
                        "fg",
                        mColourPath chunkBackground,
                        "bg"
                      ]
                    )
                    <> ".dat"
            pure (name, path, Chunk {..})

      forM_ (chunks "Hello world") $ \(name, path, c) ->
        it (unwords ["outputs a", name, "the same way as before"]) $
          pureGoldenByteStringFile (gf256 path) (renderChunkBS With256Colours c)

colourName :: Colour -> String
colourName =
  unwords . \case
    Colour8 intensity terminalColour ->
      [ show intensity,
        show terminalColour
      ]
    Colour256 w ->
      [ "8-bit colour",
        show w
      ]

mColourName :: Maybe Colour -> String
mColourName Nothing = "no"
mColourName (Just c) = "a " <> colourName c

colourPath :: Colour -> FilePath
colourPath =
  map Char.toLower
    . intercalate "-"
    . \case
      Colour8 intensity terminalColour ->
        [ show intensity,
          show terminalColour
        ]
      Colour256 w -> [show w]

mColourPath :: Maybe Colour -> FilePath
mColourPath Nothing = "no"
mColourPath (Just c) = colourPath c
