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
      pureGoldenByteStringFile (gf "reset.dat") (renderCSIBS (SGR [Reset]))
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
              pureGoldenByteStringFile (gf8 path) (renderCSIBS (SGR [sgr]))
        it "outputs an dull red background with bright blue foreground the same as before" $ do
          pureGoldenByteStringFile
            (gf8 "two-colours.dat")
            ( renderCSIBS
                ( SGR
                    [ SetColour Dull Background Red,
                      SetColour Bright Foreground Blue
                    ]
                )
            )
        it "outputs an bold, italic, underlined, dull yellow background with bright green foreground the same as before" $ do
          pureGoldenByteStringFile
            (gf8 "complex.dat")
            ( renderCSIBS
                ( SGR
                    [ SetItalic True,
                      SetUnderlining SingleUnderline,
                      SetConsoleIntensity BoldIntensity,
                      SetColour Dull Background Yellow,
                      SetColour Bright Foreground Green
                    ]
                )
            )
      let gf256 = ("test_resources/csi/256/" ++)
      describe "256 colours" $ do
        -- https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
        it "outputs a pink foreground colour with light blue background the same as before" $ do
          pureGoldenByteStringFile
            (gf256 "two-colours.dat")
            ( renderCSIBS
                ( SGR
                    [ SetColour Dull Background Red,
                      SetColour Bright Foreground Blue
                    ]
                )
            )
