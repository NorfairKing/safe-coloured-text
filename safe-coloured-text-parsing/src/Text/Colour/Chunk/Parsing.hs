{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing ANSI escape codes from text into styled 'Chunk's.
--
-- This module is the inverse of 'Text.Colour.Chunk': instead of rendering
-- 'Chunk's to ANSI-escaped 'Text', it parses ANSI-escaped 'Text' back into
-- 'Chunk's.
--
-- The parsing happens in two layers:
--
-- 1. __Tokenization__: 'Text' is split into 'AnsiToken's — plain text
--    segments and ANSI escape sequences.
-- 2. __Interpretation__: 'AnsiToken's are folded into styled 'Chunk's by
--    tracking the current SGR state.
--
-- Both layers support lazy 'Data.Text.Lazy.Text' for streaming.
module Text.Colour.Chunk.Parsing
  ( -- * High-level API
    parseAnsiChunks,
    parseAnsiChunksLazy,

    -- * Two-layer parsing

    -- ** Layer 1: Tokenization
    AnsiToken (..),
    ansiTokenP,
    parseAnsiTokens,
    parseAnsiTokensLazy,

    -- ** Layer 2: Token interpretation
    tokensToChunks,
  )
where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
  ( Parser,
    anyChar,
    char,
    endOfInput,
    many',
    parseOnly,
    peekChar',
    satisfy,
    takeWhile1,
  )
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Validity
import Data.Word
import GHC.Generics (Generic)
import Text.Colour.Chunk
import Text.Colour.Code
import Text.Read (readMaybe)

-- | A token in a stream of text with ANSI escape codes.
data AnsiToken
  = -- | Plain text (no escape codes)
    PlainText !Text
  | -- | An SGR (Select Graphic Rendition) sequence with its parameter bytes
    SgrSequence ![Word8]
  | -- | A non-SGR CSI sequence (to be discarded)
    OtherCsiSequence
  | -- | A carriage return (@\\r@)
    CarriageReturn
  deriving (Show, Eq, Generic)

instance Validity AnsiToken

-- | Parse strict 'Text' into a list of 'AnsiToken's.
-- Any incomplete escape sequence at the end of input is emitted as 'PlainText'.
parseAnsiTokens :: Text -> [AnsiToken]
parseAnsiTokens t =
  case parseOnly (many' ansiTokenP <* endOfInput) t of
    Left _ -> [PlainText t]
    Right tokens -> tokens

-- | Parse lazy 'Lazy.Text' into a list of 'AnsiToken's, produced lazily.
-- Tokens are yielded as soon as enough input has been consumed to complete them.
parseAnsiTokensLazy :: Lazy.Text -> [AnsiToken]
parseAnsiTokensLazy = go
  where
    go remaining
      | Lazy.null remaining = []
      | otherwise =
          case AL.parse ansiTokenP remaining of
            AL.Done rest token -> token : go rest
            AL.Fail _ _ _ ->
              -- This shouldn't happen since ansiTokenP always succeeds on
              -- non-empty input (incompleteEscapeP and plainTextP are catch-alls).
              [PlainText (Lazy.toStrict remaining)]

-- | Parse a single ANSI token: a CSI sequence, a bare ESC, a carriage
-- return, or plain text.
ansiTokenP :: Parser AnsiToken
ansiTokenP = csiSequenceP <|> incompleteEscapeP <|> carriageReturnP <|> plainTextP

plainTextP :: Parser AnsiToken
plainTextP = PlainText <$> takeWhile1 (\c -> c /= '\ESC' && c /= '\r')

carriageReturnP :: Parser AnsiToken
carriageReturnP = do
  _ <- char '\r'
  pure CarriageReturn

-- | Parse a lone ESC (possibly followed by '[') that doesn't start a valid CSI sequence.
incompleteEscapeP :: Parser AnsiToken
incompleteEscapeP = do
  _ <- char '\ESC'
  mBracket <- (char '[' >> pure True) <|> pure False
  pure $ PlainText $ if mBracket then "\ESC[" else "\ESC"

csiSequenceP :: Parser AnsiToken
csiSequenceP = do
  _ <- char '\ESC'
  _ <- char '['
  (params, hasIntermediateBytes) <- csiParamsP
  finalByte <- satisfy (\c -> c >= '\x40' && c <= '\x7E')
  pure $
    if finalByte == 'm' && not hasIntermediateBytes
      then SgrSequence params
      else OtherCsiSequence

csiParamsP :: Parser ([Word8], Bool)
csiParamsP = do
  (revAcc, hasIntermediate) <- go [] False
  pure (reverse revAcc, hasIntermediate)
  where
    go :: [Word8] -> Bool -> Parser ([Word8], Bool)
    go acc hasIntermediate = do
      c <- peekChar'
      if isDigit c
        then do
          digits <- takeWhile1 isDigit
          case readMaybe (Text.unpack digits) of
            Just n
              | n <= (255 :: Word) -> go (fromIntegral n : acc) hasIntermediate
            _ -> go acc hasIntermediate
        else
          if c == ';'
            then anyChar >> go acc hasIntermediate
            else
              if c >= '\x20' && c <= '\x2F'
                then anyChar >> go acc True
                else pure (acc, hasIntermediate)

-- | Convert a stream of 'AnsiToken's into styled 'Chunk's.
--
-- The first argument is the initial style state.
-- Returns the final style state and the list of chunks.
--
-- The output list is produced lazily: each 'Chunk' is yielded as soon as
-- the corresponding 'PlainText' token is encountered.  The final style
-- state is only available after the entire input has been consumed.
tokensToChunks :: Chunk -> [AnsiToken] -> (Chunk, [Chunk])
tokensToChunks style tokens =
  let (finalStyle, chunks) = go style tokens
   in (finalStyle, chunks)
  where
    go :: Chunk -> [AnsiToken] -> (Chunk, [Chunk])
    go s [] = (s, [])
    go s (token : rest) = case token of
      PlainText t
        | Text.null t -> go s rest
        | otherwise ->
            let (finalS, restChunks) = go s rest
             in (finalS, s {chunkText = t} : restChunks)
      SgrSequence params -> go (applySGRParams s params) rest
      OtherCsiSequence -> go s rest
      CarriageReturn ->
        -- In a terminal, \r moves the cursor to column 0.
        -- If followed by plain text, treat it as a line break
        -- (progress update). Otherwise discard it (terminal noise).
        case rest of
          (PlainText _ : _) ->
            let (finalS, restChunks) = go s rest
             in (finalS, s {chunkText = "\n"} : restChunks)
          _ -> go s rest

-- | Parse strict 'Text' containing ANSI escape codes into styled 'Chunk's.
--
-- The first argument is the current style state (a 'Chunk' with empty text
-- whose style fields carry over). The returned 'Chunk' is the style state
-- after processing all escape codes, suitable for threading to the next line.
parseAnsiChunks :: Chunk -> Text -> (Chunk, [Chunk])
parseAnsiChunks style = tokensToChunks style . parseAnsiTokens

-- | Parse lazy 'Lazy.Text' containing ANSI escape codes into styled 'Chunk's.
--
-- Both the token stream and the chunk list are produced lazily.
parseAnsiChunksLazy :: Chunk -> Lazy.Text -> (Chunk, [Chunk])
parseAnsiChunksLazy style = tokensToChunks style . parseAnsiTokensLazy

-- | Apply a list of SGR parameter bytes to a style 'Chunk'.
-- An empty parameter list is equivalent to reset (SGR 0).
applySGRParams :: Chunk -> [Word8] -> Chunk
applySGRParams style [] = applyReset style
applySGRParams style params = goSGR style params

goSGR :: Chunk -> [Word8] -> Chunk
goSGR s [] = s
goSGR s (p : ps)
  | p == 0 = goSGR (applyReset s) ps
  | p == 1 = goSGR (s {chunkConsoleIntensity = Just BoldIntensity}) ps
  | p == 2 = goSGR (s {chunkConsoleIntensity = Just FaintIntensity}) ps
  | p == 3 = goSGR (s {chunkItalic = Just True}) ps
  | p == 4 = goSGR (s {chunkUnderlining = Just SingleUnderline}) ps
  | p == 5 = goSGR (s {chunkBlinking = Just SlowBlinking}) ps
  | p == 6 = goSGR (s {chunkBlinking = Just RapidBlinking}) ps
  | p == 7 = goSGR (s {chunkSwapForegroundBackground = Just True}) ps
  | p == 8 = goSGR (s {chunkConcealed = Just True}) ps
  | p == 9 = goSGR (s {chunkStrikethrough = Just True}) ps
  | p == 21 = goSGR (s {chunkUnderlining = Just DoubleUnderline}) ps
  | p == 22 = goSGR (s {chunkConsoleIntensity = Just NormalIntensity}) ps
  | p == 23 = goSGR (s {chunkItalic = Just False}) ps
  | p == 24 = goSGR (s {chunkUnderlining = Just NoUnderline}) ps
  | p == 25 = goSGR (s {chunkBlinking = Just NoBlinking}) ps
  | p == 27 = goSGR (s {chunkSwapForegroundBackground = Just False}) ps
  | p == 28 = goSGR (s {chunkConcealed = Just False}) ps
  | p == 29 = goSGR (s {chunkStrikethrough = Just False}) ps
  | p >= 30 && p <= 37 =
      case terminalColourFromIndex (p - 30) of
        Just tc -> goSGR (s {chunkForeground = Just (Colour8 Dull tc)}) ps
        Nothing -> goSGR s ps
  -- Extended foreground colour
  | p == 38 = case ps of
      5 : n : rest -> goSGR (s {chunkForeground = Just (Colour8Bit n)}) rest
      2 : r : g : b : rest -> goSGR (s {chunkForeground = Just (Colour24Bit r g b)}) rest
      _ -> goSGR s ps
  -- Default foreground
  | p == 39 = goSGR (s {chunkForeground = Nothing}) ps
  -- Standard background colours (40-47)
  | p >= 40 && p <= 47 =
      case terminalColourFromIndex (p - 40) of
        Just tc -> goSGR (s {chunkBackground = Just (Colour8 Dull tc)}) ps
        Nothing -> goSGR s ps
  -- Extended background colour
  | p == 48 = case ps of
      5 : n : rest -> goSGR (s {chunkBackground = Just (Colour8Bit n)}) rest
      2 : r : g : b : rest -> goSGR (s {chunkBackground = Just (Colour24Bit r g b)}) rest
      _ -> goSGR s ps
  -- Default background
  | p == 49 = goSGR (s {chunkBackground = Nothing}) ps
  | p == 53 = goSGR (s {chunkOverlined = Just True}) ps
  | p == 55 = goSGR (s {chunkOverlined = Just False}) ps
  | p >= 90 && p <= 97 =
      case terminalColourFromIndex (p - 90) of
        Just tc -> goSGR (s {chunkForeground = Just (Colour8 Bright tc)}) ps
        Nothing -> goSGR s ps
  -- Bright background colours (100-107)
  | p >= 100 && p <= 107 =
      case terminalColourFromIndex (p - 100) of
        Just tc -> goSGR (s {chunkBackground = Just (Colour8 Bright tc)}) ps
        Nothing -> goSGR s ps
  -- Unknown code, skip
  | otherwise = goSGR s ps

applyReset :: Chunk -> Chunk
applyReset _ = chunk ""
