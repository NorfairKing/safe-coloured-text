{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Text.Colour.Chunk.ParsingSpec (spec) where

import Data.GenValidity.Text ()
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Test.Syd
import Test.Syd.Validity
import Text.Colour.Capabilities
import Text.Colour.Chunk
import Text.Colour.Chunk.Parsing
import Text.Colour.Chunk.Parsing.Gen ()
import Text.Colour.Code
import Text.Colour.Gen ()

spec :: Spec
spec = do
  genValidSpec @AnsiToken

  describe "parseAnsiTokens" $ do
    it "tokenizes plain text" $
      parseAnsiTokens "hello" `shouldBe` [PlainText "hello"]

    it "tokenizes an SGR sequence" $
      parseAnsiTokens "\ESC[31m" `shouldBe` [SgrSequence [31]]

    it "tokenizes mixed content" $
      parseAnsiTokens "hello\ESC[31mworld\ESC[0m!"
        `shouldBe` [PlainText "hello", SgrSequence [31], PlainText "world", SgrSequence [0], PlainText "!"]

    it "tokenizes non-SGR CSI as OtherCsiSequence" $
      parseAnsiTokens "\ESC[2J" `shouldBe` [OtherCsiSequence]

    it "treats CSI with intermediate bytes as non-SGR" $
      parseAnsiTokens "\ESC[ 1m" `shouldBe` [OtherCsiSequence]

    it "tokenizes combined parameters" $
      parseAnsiTokens "\ESC[1;31m" `shouldBe` [SgrSequence [1, 31]]

    it "tokenizes 256-colour parameters" $
      parseAnsiTokens "\ESC[38;5;196m" `shouldBe` [SgrSequence [38, 5, 196]]

    it "tokenizes bare ESC as plain text" $
      parseAnsiTokens "before\ESCafter"
        `shouldBe` [PlainText "before", PlainText "\ESC", PlainText "after"]

    it "keeps incomplete ESC[ together as single token" $
      parseAnsiTokens "hello\ESC[" `shouldBe` [PlainText "hello", PlainText "\ESC["]

    it "drops parameter values that overflow Word8" $
      parseAnsiTokens "\ESC[256m" `shouldBe` [SgrSequence []]

    it "tokenizes empty input" $
      parseAnsiTokens "" `shouldBe` []

  describe "parseAnsiTokensLazy" $ do
    it "produces the same tokens as strict parsing" $
      forAllValid $ \text ->
        parseAnsiTokensLazy (Lazy.fromStrict text) `shouldBe` parseAnsiTokens text

    it "works with multi-chunk lazy text" $
      let lazyText = Lazy.fromChunks ["hello\ESC[31m", "world\ESC[0m!"]
          tokens = parseAnsiTokensLazy lazyText
       in tokens
            `shouldBe` [PlainText "hello", SgrSequence [31], PlainText "world", SgrSequence [0], PlainText "!"]

  describe "parseAnsiChunks" $ do
    it "returns plain text unchanged" $
      parseAnsiChunks (chunk "") "hello world"
        `shouldBe` (chunk "", [chunk "hello world"])

    it "parses a simple red foreground" $
      parseAnsiChunks (chunk "") "\ESC[31mhello\ESC[0m world"
        `shouldBe` ( chunk "",
                     [ fore (Colour8 Dull Red) (chunk "hello"),
                       chunk " world"
                     ]
                   )

    it "parses bold" $
      parseAnsiChunks (chunk "") "\ESC[1mbold\ESC[0m"
        `shouldBe` (chunk "", [bold (chunk "bold")])

    it "parses combined bold and colour" $
      parseAnsiChunks (chunk "") "\ESC[1;31mred bold\ESC[0m"
        `shouldBe` (chunk "", [fore (Colour8 Dull Red) (bold (chunk "red bold"))])

    it "parses bright colours" $
      parseAnsiChunks (chunk "") "\ESC[91mbright red\ESC[0m"
        `shouldBe` (chunk "", [fore (Colour8 Bright Red) (chunk "bright red")])

    it "parses 256-colour foreground" $
      parseAnsiChunks (chunk "") "\ESC[38;5;196mcolour\ESC[0m"
        `shouldBe` (chunk "", [fore (Colour8Bit 196) (chunk "colour")])

    it "parses 24-bit RGB foreground" $
      parseAnsiChunks (chunk "") "\ESC[38;2;255;128;0mcolour\ESC[0m"
        `shouldBe` (chunk "", [fore (Colour24Bit 255 128 0) (chunk "colour")])

    it "parses background colour" $
      parseAnsiChunks (chunk "") "\ESC[42mgreen bg\ESC[0m"
        `shouldBe` (chunk "", [back (Colour8 Dull Green) (chunk "green bg")])

    it "strips non-SGR CSI sequences" $
      parseAnsiChunks (chunk "") "before\ESC[2Jafter"
        `shouldBe` (chunk "", [chunk "before", chunk "after"])

    it "handles empty input" $
      parseAnsiChunks (chunk "") ""
        `shouldBe` (chunk "", [])

    it "handles text with no visible content between escapes" $
      parseAnsiChunks (chunk "") "\ESC[31m\ESC[0m"
        `shouldBe` (chunk "", [])

    it "threads state across calls" $
      let (style1, chunks1) = parseAnsiChunks (chunk "") "\ESC[31mhello"
          result2 = parseAnsiChunks style1 "world\ESC[0m"
       in do
            chunks1 `shouldBe` [fore (Colour8 Dull Red) (chunk "hello")]
            style1 `shouldBe` fore (Colour8 Dull Red) (chunk "")
            result2 `shouldBe` (chunk "", [fore (Colour8 Dull Red) (chunk "world")])

    it "handles incomplete sequence at end of text" $
      parseAnsiChunks (chunk "") "hello\ESC["
        `shouldBe` (chunk "", [chunk "hello", chunk "\ESC["])

    it "never crashes on arbitrary input" $
      forAllValid $ \text ->
        let (_, chunks) = parseAnsiChunks (chunk "") text
         in seq (length chunks) (pure () :: IO ())

    it "produces segment texts that concatenate to the input minus ANSI codes" $
      forAllValid $ \text ->
        let (_, chunks) = parseAnsiChunks (chunk "") text
            combined = Text.concat $ map chunkText chunks
         in combined `shouldBe` stripAnsi text

  describe "parseAnsiChunksLazy" $ do
    it "produces the same chunks as strict parsing" $
      forAllValid $ \text ->
        let (styleS, chunksS) = parseAnsiChunks (chunk "") text
            (styleL, chunksL) = parseAnsiChunksLazy (chunk "") (Lazy.fromStrict text)
         in do
              chunksL `shouldBe` chunksS
              styleL `shouldBe` styleS

  describe "roundtrip" $ do
    it "recovers chunk style from rendered output for non-empty chunks" $
      forAllValid $ \c ->
        let c' = c {chunkText = if Text.null (chunkText c) then "x" else chunkText c}
            rendered = renderChunkText With24BitColours c'
            (_, parsed) = parseAnsiChunks (chunk "") rendered
         in parsed `shouldBe` [c']

-- | Strip all ANSI CSI sequences from text.
stripAnsi :: Text.Text -> Text.Text
stripAnsi t = case Text.breakOn "\ESC[" t of
  (prefix, suffix)
    | Text.null suffix -> prefix
    | otherwise ->
        let rest = Text.drop 2 suffix
         in case Text.findIndex (\c -> c >= '\x40' && c <= '\x7E') rest of
              Nothing -> prefix <> suffix
              Just i -> prefix <> stripAnsi (Text.drop (i + 1) rest)
