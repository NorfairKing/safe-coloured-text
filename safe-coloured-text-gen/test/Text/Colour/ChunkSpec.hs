{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Colour.ChunkSpec (spec) where

import Control.Monad
import Data.Char as Char
import Data.List
import qualified Data.Text as T
import Test.Syd
import Text.Colour.Chunk

spec :: Spec
spec = do
  let gf = ("test_resources/chunk/" ++)
  describe "renderChunk" $ do
    it "outputs a plain chunk the same as before" $
      pureGoldenByteStringFile (gf "plain.dat") (renderChunkBS (chunk "Hello world"))
    let chunks string = do
          let colour = do
                terminalColour <- [minBound .. maxBound]
                intensity <- [minBound .. maxBound]
                pure $ Colour intensity terminalColour
          let mColour = Nothing : map Just colour
          chunkForeground <- mColour
          chunkBackground <- mColour
          let chunkText = T.pack string
          let colourName (Colour i tc) = unwords [show i, show tc]
              mColourName :: Maybe Colour -> String
              mColourName Nothing = "no"
              mColourName (Just c) = "a " <> colourName c
              name = unwords [string, "with", mColourName chunkForeground, "foreground on", mColourName chunkBackground, "background"]
              colourPath (Colour i tc) = map Char.toLower $ intercalate "-" [show i, show tc]
              mColourPath :: Maybe Colour -> FilePath
              mColourPath Nothing = "no"
              mColourPath (Just c) = colourPath c
              path = intercalate "-" [mColourPath chunkForeground, "fg", mColourPath chunkBackground, "bg"] <> ".dat"
          pure (name, path, Chunk {..})

    forM_ (chunks "Hello world") $ \(name, path, c) ->
      it (unwords ["outputs a", name, "the same way as before"]) $
        pureGoldenByteStringFile (gf path) (renderChunkBS c)
