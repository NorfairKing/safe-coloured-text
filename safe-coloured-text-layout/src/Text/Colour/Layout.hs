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
      tableBackgroundColour = Nothing
    }

data Table = Table
  { -- | A list of rows. They must be of the same length.
    tableCells :: [[Chunk]],
    tableColumnSeparator :: Chunk,
    tableBackgroundColour :: Maybe Colour
  }
  deriving (Show, Eq, Generic)

-- TODO spacing
-- TODO single-colour background
-- TODO bicolour background

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
      withBg :: Chunk -> Chunk
      withBg = possiblyAddBackground tableBackgroundColour
      renderRow :: [(Chunk, Chunk)] -> [Chunk]
      renderRow [] = ["\n"]
      renderRow [(c, p)] = withBg c : withBg p : renderRow []
      renderRow ((c1, p1) : t2 : rest) = withBg c1 : withBg p1 : withBg tableColumnSeparator : renderRow (t2 : rest)
   in concatMap renderRow paddedRows

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