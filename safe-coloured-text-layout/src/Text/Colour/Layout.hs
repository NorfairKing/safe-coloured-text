{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Colour.Layout where

import Control.Applicative
import Data.List
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Colour

layoutAsTable :: [[Chunk]] -> [Chunk]
layoutAsTable = renderTable . table

table :: [[Chunk]] -> Table
table cs =
  Table
    { tableCells = cs,
      tableColumnSeparator = " ",
      tableBackground = Nothing
    }

data Table = Table
  { -- | A list of rows. They must be of the same length.
    tableCells :: [[Chunk]],
    tableColumnSeparator :: Chunk,
    tableBackground :: Maybe TableBackground
  }
  deriving (Show, Eq, Generic)

data TableBackground
  = SingleColour Colour
  | Bicolour
      (Maybe Colour) -- Even-numbered table rows (0-indexed)
      (Maybe Colour) -- Odd-numbered table rows
  deriving (Show, Eq, Generic)

renderTable :: Table -> [Chunk]
renderTable Table {..} =
  let asColumns = transpose (padRows tableCells)
      addLengthsToColumn :: [Chunk] -> [(Int, Chunk)]
      addLengthsToColumn = map (\c -> (T.length (chunkText c), c))
      maxLengthOfColum :: [(Int, Chunk)] -> Int
      maxLengthOfColum = maximum . map fst
      padColumn :: Int -> [(Int, Chunk)] -> [(Chunk, Chunk)]
      padColumn maxLength = map (\(l, c) -> (c, paddingChunk (maxLength - l) ' '))
      padEntireColumn :: [(Int, Chunk)] -> [(Chunk, Chunk)]
      padEntireColumn col =
        let maxLength = maxLengthOfColum col
         in padColumn maxLength col
      paddedColumns :: [[(Chunk, Chunk)]]
      paddedColumns = map (padEntireColumn . addLengthsToColumn) asColumns
      paddedRows :: [[(Chunk, Chunk)]]
      paddedRows = transpose paddedColumns
      withBg :: Int -> Chunk -> Chunk
      withBg i = possiblyAddBackground $ backgroundForRow i tableBackground
      renderRow :: Int -> [(Chunk, Chunk)] -> [Chunk]
      renderRow i = go
        where
          go [] = ["\n"]
          go [(c, p)] = withBg i c : withBg i p : go []
          go ((c1, p1) : t2 : rest) = withBg i c1 : withBg i p1 : withBg i tableColumnSeparator : go (t2 : rest)
   in concat $ iterateLikeInPython renderRow paddedRows

iterateLikeInPython :: (Int -> a -> b) -> [a] -> [b]
iterateLikeInPython f = zipWith f [0 ..]

padRows :: [[Chunk]] -> [[Chunk]]
padRows [] = []
padRows css =
  let withLengths = map (\ls -> (length ls, ls)) css
      maximumLength = maximum $ map fst withLengths
      pad (l, cs) = cs ++ replicate (maximumLength - l) ""
   in map pad withLengths

paddingChunk :: Int -> Char -> Chunk
paddingChunk l c = chunk $ T.pack $ replicate l c

possiblyAddBackground :: Maybe Colour -> Chunk -> Chunk
possiblyAddBackground mb c = c {chunkBackground = chunkBackground c <|> mb}

backgroundForRow :: Int -> Maybe TableBackground -> Maybe Colour
backgroundForRow _ Nothing = Nothing
backgroundForRow _ (Just (SingleColour c)) = Just c
backgroundForRow i (Just (Bicolour ec oc)) = if even i then ec else oc
