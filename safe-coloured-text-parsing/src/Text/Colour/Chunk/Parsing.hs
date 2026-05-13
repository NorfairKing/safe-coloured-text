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
    OscCommand (..),
    ansiTokenP,
    parseAnsiTokens,
    parseAnsiTokensLazy,

    -- ** Layer 2: Token interpretation
    tokensToChunks,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
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

-- | The payload of an OSC (Operating System Command) escape sequence.
data OscCommand
  = -- | OSC 8 hyperlink.
    -- @'OscHyperlink' params uri@: @params@ are optional link attributes
    -- (commonly empty); @uri@ is the target URL, or empty to close the link.
    OscHyperlink !Text !Text
  | -- | Any other OSC sequence, identified by its command number and
    -- the raw parameter string that followed it.
    OscOther !Word !Text
  deriving (Show, Eq, Generic)

instance Validity OscCommand

-- | A token in a stream of text with ANSI escape codes.
data AnsiToken
  = -- | Plain text (no escape codes)
    PlainText !Text
  | -- | An SGR (Select Graphic Rendition) sequence with its parameter bytes
    SgrSequence ![Word8]
  | -- | A non-SGR CSI sequence (to be discarded)
    OtherCsiSequence
  | -- | An OSC (Operating System Command) sequence.
    -- Begins with @ESC ]@ and ends with the String Terminator @ESC \@ or @BEL@.
    OscSequence !OscCommand
  | -- | A carriage return (@\r@)
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

-- | Parse a single ANSI token: a CSI sequence, an OSC sequence, a bare ESC,
-- a carriage return, or plain text.
ansiTokenP :: Parser AnsiToken
ansiTokenP = csiSequenceP <|> oscSequenceP <|> incompleteEscapeP <|> carriageReturnP <|> plainTextP

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

-- | Parse an OSC (Operating System Command) sequence.
-- An OSC sequence begins with @ESC ]@, followed by a numeric command,
-- a @;@, a parameter string, and ends with the String Terminator @ESC \@
-- or @BEL@ (@\a@).
-- If no terminator is found before end of input the whole remainder
-- is consumed and emitted as 'OscSequence'.
oscSequenceP :: Parser AnsiToken
oscSequenceP = do
  _ <- char '\ESC'
  _ <- char ']'
  cmd <- oscCommandNumberP
  _ <- void (char ';') <|> pure ()
  params <- oscParamsP
  oscStringTerminatorP
  pure $ OscSequence $ case cmd of
    8 -> case Text.breakOn ";" params of
      (linkParams, rest)
        | not (Text.null rest) -> OscHyperlink linkParams (Text.drop 1 rest)
      _ -> OscHyperlink "" params
    _ -> OscOther cmd params

-- | Parse an OSC command number (a decimal integer).
-- Returns 0 if no digits are present (bare @ESC]@).
oscCommandNumberP :: Parser Word
oscCommandNumberP =
  ( do
      digits <- takeWhile1 isDigit
      case readMaybe (Text.unpack digits) of
        Just n -> pure n
        Nothing -> pure 0
  )
    <|> pure 0

-- | Parse the OSC parameter string: everything up to the string terminator.
oscParamsP :: Parser Text
oscParamsP = do
  chunks <- many' (satisfy (\c -> c /= '\ESC' && c /= '\BEL'))
  pure $ Text.pack chunks

-- | Consume the OSC string terminator: @ESC \@ or @BEL@.
-- If neither is present (end of input), succeeds consuming nothing.
oscStringTerminatorP :: Parser ()
oscStringTerminatorP =
  (char '\ESC' >> void (char '\\'))
    <|> void (char '\BEL')
    <|> pure ()

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
tokensToChunks :: ChunkStyle -> [AnsiToken] -> (ChunkStyle, [Chunk])
tokensToChunks style tokens =
  let (finalStyle, chunks) = go style tokens
   in (finalStyle, chunks)
  where
    go :: ChunkStyle -> [AnsiToken] -> (ChunkStyle, [Chunk])
    go s [] = (s, [])
    go s (token : rest) = case token of
      PlainText t
        | Text.null t -> go s rest
        | otherwise ->
            let (finalS, restChunks) = go s rest
             in (finalS, Chunk {chunkText = t, chunkStyle = s} : restChunks)
      SgrSequence params -> go (applySGRParams s params) rest
      OtherCsiSequence -> go s rest
      OscSequence (OscHyperlink _ url) ->
        go (s {chunkStyleHyperlink = Just url}) rest
      OscSequence _ -> go s rest
      CarriageReturn ->
        case rest of
          (PlainText _ : _) ->
            let (finalS, restChunks) = go s rest
             in (finalS, Chunk {chunkText = "\n", chunkStyle = s} : restChunks)
          _ -> go s rest

-- | Parse strict 'Text' containing ANSI escape codes into styled 'Chunk's.
--
-- The first argument is the current style state. The returned 'ChunkStyle'
-- is the style state after processing all escape codes, suitable for
-- threading to the next line.
parseAnsiChunks :: ChunkStyle -> Text -> (ChunkStyle, [Chunk])
parseAnsiChunks style = tokensToChunks style . parseAnsiTokens

-- | Parse lazy 'Lazy.Text' containing ANSI escape codes into styled 'Chunk's.
--
-- Both the token stream and the chunk list are produced lazily.
parseAnsiChunksLazy :: ChunkStyle -> Lazy.Text -> (ChunkStyle, [Chunk])
parseAnsiChunksLazy style = tokensToChunks style . parseAnsiTokensLazy

-- | Apply a list of SGR parameter bytes to a 'ChunkStyle'.
-- An empty parameter list is equivalent to reset (SGR 0).
applySGRParams :: ChunkStyle -> [Word8] -> ChunkStyle
applySGRParams _ [] = noStyle
applySGRParams style params = goSGR style params

goSGR :: ChunkStyle -> [Word8] -> ChunkStyle
goSGR s [] = s
goSGR s (p : ps)
  | p == 0 = goSGR noStyle ps
  | p == 1 = goSGR (s {chunkStyleConsoleIntensity = Just BoldIntensity}) ps
  | p == 2 = goSGR (s {chunkStyleConsoleIntensity = Just FaintIntensity}) ps
  | p == 3 = goSGR (s {chunkStyleItalic = Just True}) ps
  | p == 4 = goSGR (s {chunkStyleUnderlining = Just SingleUnderline}) ps
  | p == 5 = goSGR (s {chunkStyleBlinking = Just SlowBlinking}) ps
  | p == 6 = goSGR (s {chunkStyleBlinking = Just RapidBlinking}) ps
  | p == 7 = goSGR (s {chunkStyleSwapForegroundBackground = Just True}) ps
  | p == 8 = goSGR (s {chunkStyleConcealed = Just True}) ps
  | p == 9 = goSGR (s {chunkStyleStrikethrough = Just True}) ps
  | p == 21 = goSGR (s {chunkStyleUnderlining = Just DoubleUnderline}) ps
  | p == 22 = goSGR (s {chunkStyleConsoleIntensity = Just NormalIntensity}) ps
  | p == 23 = goSGR (s {chunkStyleItalic = Just False}) ps
  | p == 24 = goSGR (s {chunkStyleUnderlining = Just NoUnderline}) ps
  | p == 25 = goSGR (s {chunkStyleBlinking = Just NoBlinking}) ps
  | p == 27 = goSGR (s {chunkStyleSwapForegroundBackground = Just False}) ps
  | p == 28 = goSGR (s {chunkStyleConcealed = Just False}) ps
  | p == 29 = goSGR (s {chunkStyleStrikethrough = Just False}) ps
  -- Standard foreground colours (30-37)
  | p >= 30 && p <= 37 =
      case terminalColourFromIndex (p - 30) of
        Just tc -> goSGR (s {chunkStyleForeground = Just (Colour8 Dull tc)}) ps
        Nothing -> goSGR s ps
  -- Extended foreground colour
  | p == 38 = case ps of
      5 : n : rest -> goSGR (s {chunkStyleForeground = Just (Colour8Bit n)}) rest
      2 : r : g : b : rest -> goSGR (s {chunkStyleForeground = Just (Colour24Bit r g b)}) rest
      _ -> goSGR s ps
  -- Default foreground
  | p == 39 = goSGR (s {chunkStyleForeground = Nothing}) ps
  -- Standard background colours (40-47)
  | p >= 40 && p <= 47 =
      case terminalColourFromIndex (p - 40) of
        Just tc -> goSGR (s {chunkStyleBackground = Just (Colour8 Dull tc)}) ps
        Nothing -> goSGR s ps
  -- Extended background colour
  | p == 48 = case ps of
      5 : n : rest -> goSGR (s {chunkStyleBackground = Just (Colour8Bit n)}) rest
      2 : r : g : b : rest -> goSGR (s {chunkStyleBackground = Just (Colour24Bit r g b)}) rest
      _ -> goSGR s ps
  -- Default background
  | p == 49 = goSGR (s {chunkStyleBackground = Nothing}) ps
  | p == 53 = goSGR (s {chunkStyleOverlined = Just True}) ps
  | p == 55 = goSGR (s {chunkStyleOverlined = Just False}) ps
  -- Bright foreground colours (90-97)
  | p >= 90 && p <= 97 =
      case terminalColourFromIndex (p - 90) of
        Just tc -> goSGR (s {chunkStyleForeground = Just (Colour8 Bright tc)}) ps
        Nothing -> goSGR s ps
  -- Bright background colours (100-107)
  | p >= 100 && p <= 107 =
      case terminalColourFromIndex (p - 100) of
        Just tc -> goSGR (s {chunkStyleBackground = Just (Colour8 Bright tc)}) ps
        Nothing -> goSGR s ps
  -- Unknown code, skip
  | otherwise = goSGR s ps
