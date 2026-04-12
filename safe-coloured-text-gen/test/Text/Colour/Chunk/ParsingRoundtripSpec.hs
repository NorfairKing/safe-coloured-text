{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Colour.Chunk.ParsingRoundtripSpec (spec) where

import Control.Monad
import qualified Data.ByteString as SB
import Data.List (sort)
import qualified Data.Text.Encoding as TE
import Path
import Path.IO (listDirRecurRel, resolveDir')
import Test.Syd
import Text.Colour.Capabilities
import Text.Colour.Chunk
import Text.Colour.Chunk.Parsing

spec :: Spec
spec = do
  resourceDir <- liftIO $ resolveDir' "test_resources"
  let chunkDir = resourceDir </> [reldir|chunk|]
  files <- liftIO $ sort . snd <$> listDirRecurRel chunkDir
  describe "golden file roundtrip" $
    forM_ files $ \file ->
      it ("roundtrips " ++ fromRelFile file) $ do
        bs <- SB.readFile (fromAbsFile (chunkDir </> file))
        content <- case TE.decodeUtf8' bs of
          Left err -> expectationFailure $ "Invalid UTF-8: " ++ show err
          Right t -> pure t
        let (_, parsed) = parseAnsiChunks noStyle content
            reRendered = renderChunksText With24BitColours parsed
        reRendered `shouldBe` content
