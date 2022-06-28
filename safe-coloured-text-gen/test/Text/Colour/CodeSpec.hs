{-# LANGUAGE OverloadedStrings #-}

module Text.Colour.CodeSpec (spec) where

import Control.Monad
import Data.Char as Char
import Data.List
import Test.Syd
import Text.Colour.Code

spec :: Spec
spec = do
  let gf = ("test_resources/csi/" ++)
  describe "renderCSI" $ do
    it "outputs a reset the same as before" $
      pureGoldenTextFile (gf "reset.dat") (renderCSIText (SGR [Reset]))
    describe "SGR" $ do
      let gf8 = ("test_resources/csi/8/" ++)
      describe "8 colours" $ do
        describe "simple, exhaustive" $ do
          let sgrTests = do
                intensity <- [minBound .. maxBound]
                layer <- [minBound .. maxBound]
                colour <- [minBound .. maxBound]
                pure
                  ( unwords [show intensity, show colour, show layer],
                    map Char.toLower (intercalate "-" [show intensity, show colour, show layer]) ++ ".dat",
                    SetColour intensity layer colour
                  )
          forM_ sgrTests $ \(name, path, sgr) ->
            it (unwords ["outputs a", show name, "the same as before"]) $
              pureGoldenTextFile (gf8 path) (renderCSIText (SGR [sgr]))
        it "outputs an dull red background with bright blue foreground the same as before" $ do
          pureGoldenTextFile
            (gf8 "two-colours.dat")
            ( renderCSIText
                ( SGR
                    [ SetColour Dull Background Red,
                      SetColour Bright Foreground Blue
                    ]
                )
            )
        it "outputs a blinking, bold, italic, underlined, dull yellow background with bright green foreground the same as before" $ do
          pureGoldenTextFile
            (gf8 "complex.dat")
            ( mconcat
                [ renderCSIText
                    ( SGR
                        [ SetItalic True,
                          SetUnderlining SingleUnderline,
                          SetBlinking SlowBlinking,
                          SetConsoleIntensity BoldIntensity,
                          SetColour Dull Background Yellow,
                          SetColour Bright Foreground Green
                        ]
                    ),
                  "hello world",
                  renderCSIText
                    (SGR [Reset])
                ]
            )
      let gf256 = ("test_resources/csi/256/" ++)
      describe "256 colours" $ do
        -- https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
        it "outputs a pink foreground colour with light blue background the same as before" $ do
          pureGoldenTextFile
            (gf256 "two-colours.dat")
            ( renderCSIText
                ( SGR
                    [ SetColour Dull Background Red,
                      SetColour Bright Foreground Blue
                    ]
                )
            )
