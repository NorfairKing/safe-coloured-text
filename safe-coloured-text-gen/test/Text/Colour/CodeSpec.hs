module Text.Colour.CodeSpec (spec) where

import Control.Monad
import Data.Char as Char
import Data.List
import Test.Syd
import Text.Colour.Code

spec :: Spec
spec = do
  let gf = ("test_resources/" ++)
  describe "renderCSI" $ do
    it "outputs a reset the same as before" $
      pureGoldenByteStringFile (gf "reset.dat") (renderCSIBS (SGR [Reset]))
    describe "SGR" $ do
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
            pureGoldenByteStringFile (gf path) (renderCSIBS (SGR [sgr]))
