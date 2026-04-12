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
    it "outputs plain text if the terminal has no colours" $
      renderChunksText WithoutColours [fore red "hello"] `shouldBe` "hello"
    it "outputs plain text if the terminal has no colours and 256 are needed" $
      renderChunksText WithoutColours [fore (colour256 128) "hello"] `shouldBe` "hello"
    it "outputs plain text if the terminal has only 8 colours and 256 are needed" $
      renderChunksText With8Colours [fore (colour256 128) "hello"] `shouldBe` "hello"
    it "outputs plain text if the terminal has no colours and 24bit colours are needed" $
      renderChunksText WithoutColours [fore (colourRGB 128 128 128) "hello"] `shouldBe` "hello"
    it "outputs plain text if the terminal has only 8 colours and 24bit colours are needed" $
      renderChunksText With8Colours [fore (colourRGB 128 128 128) "hello"] `shouldBe` "hello"
    it "outputs plain text if the terminal has only 8bit colours and 24bit colours are needed" $
      renderChunksText With8BitColours [fore (colourRGB 128 128 128) "hello"] `shouldBe` "hello"
    it "outputs a plain chunk the same as before" $
      pureGoldenTextFile (gf "plain.dat") (renderChunkText With24BitColours (chunk "ook"))
    describe "8 colours" $ do
      let gf8 = ("test_resources/chunk/8/" ++)
      let chunks string = justAFew $ do
            let colour = do
                  terminalColour <- [minBound .. maxBound]
                  intensity <- [minBound .. maxBound]
                  pure $ Colour8 intensity terminalColour
            let mColour = Nothing : map Just colour
            chunkStyleItalic <- Nothing : map Just [minBound .. maxBound]
            chunkStyleStrikethrough <- Nothing : map Just [minBound .. maxBound]
            chunkStyleSwapForegroundBackground <- Nothing : map Just [minBound .. maxBound]
            chunkStyleConcealed <- Nothing : map Just [minBound .. maxBound]
            chunkStyleOverlined <- Nothing : map Just [minBound .. maxBound]
            chunkStyleConsoleIntensity <- Nothing : map Just [minBound .. maxBound]
            chunkStyleUnderlining <- Nothing : map Just [minBound .. maxBound]
            chunkStyleBlinking <- Nothing : map Just [minBound .. maxBound]
            chunkStyleForeground <- mColour
            chunkStyleBackground <- mColour
            let chunkText = T.pack string
                chunkStyle = ChunkStyle {..}
            let boolName label b = if b then label else "non-" <> label
                boolPath label b = if b then label else "no-" <> label
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
                blinkingName :: Blinking -> String
                blinkingName = \case
                  SlowBlinking -> "slow blinking"
                  RapidBlinking -> "rapid blinking"
                  NoBlinking -> "no blinking"
                name =
                  unwords $
                    filter
                      (not . null)
                      [ maybe "" (boolName "italic") chunkStyleItalic,
                        maybe "" (boolName "strikethrough") chunkStyleStrikethrough,
                        maybe "" (boolName "reverse") chunkStyleSwapForegroundBackground,
                        maybe "" (boolName "concealed") chunkStyleConcealed,
                        maybe "" (boolName "overlined") chunkStyleOverlined,
                        maybe "" consoleIntensityName chunkStyleConsoleIntensity,
                        maybe "" underliningName chunkStyleUnderlining,
                        maybe "" blinkingName chunkStyleBlinking,
                        string,
                        "with",
                        mColourName chunkStyleForeground,
                        "foreground on",
                        mColourName chunkStyleBackground,
                        "background"
                      ]

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
                blinkingPath :: Blinking -> String
                blinkingPath = \case
                  SlowBlinking -> "slow-blinking"
                  RapidBlinking -> "rapid-blinking"
                  NoBlinking -> "no-blinking"
                path =
                  intercalate
                    "-"
                    ( filter
                        (not . null)
                        [ maybe "" (boolPath "it") chunkStyleItalic,
                          maybe "" (boolPath "st") chunkStyleStrikethrough,
                          maybe "" (boolPath "rv") chunkStyleSwapForegroundBackground,
                          maybe "" (boolPath "hd") chunkStyleConcealed,
                          maybe "" (boolPath "ol") chunkStyleOverlined,
                          maybe "" consoleIntensityPath chunkStyleConsoleIntensity,
                          maybe "" underliningPath chunkStyleUnderlining,
                          maybe "" blinkingPath chunkStyleBlinking,
                          mColourPath chunkStyleForeground,
                          "fg",
                          mColourPath chunkStyleBackground,
                          "bg"
                        ]
                    )
                    <> ".dat"
            pure (name, path, Chunk {..})

      forM_ (chunks "ook") $ \(name, path, c) ->
        it (unwords ["outputs a", name, "the same way as before"]) $
          pureGoldenTextFile (gf8 path) (renderChunkText With24BitColours c)
    describe "8bit colours" $ do
      let gf8bit = ("test_resources/chunk/8bit/" ++)
      let chunks string = justAFew $ do
            let colour = Colour8Bit <$> [minBound .. maxBound]
            let mColour = Nothing : map Just colour
            let chunkStyleItalic = Nothing
            let chunkStyleStrikethrough = Nothing
            let chunkStyleSwapForegroundBackground = Nothing
            let chunkStyleConcealed = Nothing
            let chunkStyleOverlined = Nothing
            let chunkStyleConsoleIntensity = Nothing
            let chunkStyleUnderlining = Nothing
            let chunkStyleBlinking = Nothing
            chunkStyleForeground <- mColour
            chunkStyleBackground <- mColour
            let chunkText = T.pack string
                chunkStyle = ChunkStyle {..}
            let name =
                  unwords
                    [ mColourName chunkStyleForeground,
                      "foreground on",
                      mColourName chunkStyleBackground,
                      "background"
                    ]
                path =
                  intercalate
                    "-"
                    [ mColourPath chunkStyleForeground,
                      "fg",
                      mColourPath chunkStyleBackground,
                      "bg"
                    ]
                    <> ".dat"
            pure (name, path, Chunk {..})

      forM_ (chunks "ook") $ \(name, path, c) ->
        it (unwords ["outputs a", name, "the same way as before"]) $
          pureGoldenTextFile (gf8bit path) (renderChunkText With24BitColours c)
    describe "24bit colours" $ do
      let gf24bit = ("test_resources/chunk/24bit/" ++)
      let chunks string = justAFew $ do
            let colour = do
                  let w = [0, 16 .. 255] -- Just a few colours, otherwise we end up with a boatload of files.
                  r <- w
                  g <- w
                  b <- w
                  pure $ Colour24Bit r g b
            let mColour = Nothing : map Just colour
            let chunkStyleItalic = Nothing
            let chunkStyleStrikethrough = Nothing
            let chunkStyleSwapForegroundBackground = Nothing
            let chunkStyleConcealed = Nothing
            let chunkStyleOverlined = Nothing
            let chunkStyleConsoleIntensity = Nothing
            let chunkStyleUnderlining = Nothing
            let chunkStyleBlinking = Nothing
            chunkStyleForeground <- mColour
            chunkStyleBackground <- mColour
            let chunkText = T.pack string
                chunkStyle = ChunkStyle {..}
            let name =
                  unwords
                    [ mColourName chunkStyleForeground,
                      "foreground on",
                      mColourName chunkStyleBackground,
                      "background"
                    ]
                path =
                  intercalate
                    "-"
                    [ mColourPath chunkStyleForeground,
                      "fg",
                      mColourPath chunkStyleBackground,
                      "bg"
                    ]
                    <> ".dat"
            pure (name, path, Chunk {..})

      forM_ (chunks "ook") $ \(name, path, c) ->
        it (unwords ["outputs a", name, "the same way as before"]) $
          pureGoldenTextFile (gf24bit path) (renderChunkText With24BitColours c)
    describe "super fancy" $ do
      it "outputs this super fancy thing the same way as before" $
        let bc c = back c $ chunk " "

            colour8Bits = Colour8Bit <$> [16 .. maxBound]
            colour8Bitss = (Colour8Bit <$> [0 .. 7]) : (Colour8Bit <$> [8 .. 15]) : chunksOf 36 colour8Bits
            wsWithGaps = [0, 16 .. 255]
            colour24Bits = (Colour24Bit <$> wsWithGaps <*> wsWithGaps <*> wsWithGaps)
            colour24Bitss = chunksOf 64 colour24Bits
            cs =
              intercalate
                ["\n"]
                $ concat
                  [ [ "Terminal colours (dull):   " : map (bc . Colour8 Dull) [minBound .. maxBound],
                      "Terminal colours (bright): " : map (bc . Colour8 Bright) [minBound .. maxBound]
                    ],
                    ["\n8 bit colours:"] : map (map bc) colour8Bitss,
                    ["\n24 bit colours:"] : map (map bc) colour24Bitss
                  ]
         in pureGoldenTextFile (gf "fancy.dat") (renderChunksText With24BitColours cs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf w l
  | length l > w = take w l : chunksOf w (drop w l)
  | otherwise = [l]

colourName :: Colour -> String
colourName =
  unwords . \case
    Colour8 intensity terminalColour ->
      [ show intensity,
        show terminalColour
      ]
    Colour8Bit w ->
      [ "8-bit colour",
        show w
      ]
    Colour24Bit r g b ->
      [ "8-bit colour",
        show (r, g, b)
      ]

mColourName :: Maybe Colour -> String
mColourName Nothing = "no"
mColourName (Just c) = "a(n) " <> colourName c

colourPath :: Colour -> FilePath
colourPath =
  map Char.toLower
    . intercalate "-"
    . \case
      Colour8 intensity terminalColour ->
        [ show intensity,
          show terminalColour
        ]
      Colour8Bit w -> [show w]
      Colour24Bit r g b -> [show r, show g, show b]

mColourPath :: Maybe Colour -> FilePath
mColourPath Nothing = "no"
mColourPath (Just c) = colourPath c

justAFew :: [a] -> [a]
justAFew = go 1
  where
    go _ [] = []
    go i (a : as) = a : go (2 * i) (drop i as)
