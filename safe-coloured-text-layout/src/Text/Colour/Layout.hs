{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Colour.Layout
  ( layoutAsTable,
    layoutAsTableLines,
    table,
    Table (..),
    TableBackground (..),
    renderTable,
    renderTableLines,
  )
where

import Control.Applicative
import Data.List
import qualified Data.Text as T
import Data.Validity
import GHC.Generics (Generic)
import Text.Colour
import Text.Colour.Chunk

-- | Render as a default-settings table.
layoutAsTable :: [[[Chunk]]] -> [Chunk]
layoutAsTable = renderTable . table

layoutAsTableLines :: [[[Chunk]]] -> [[Chunk]]
layoutAsTableLines = renderTableLines . table

-- | Make a table with default settings
--
-- You can then update table settings by changing the fields in the resulting 'Table'.
table :: [[[Chunk]]] -> Table
table cs =
  Table
    { tableCells = cs,
      tableColumnSeparator = " ",
      tableBackground = Nothing
    }

-- | Table with separator and background settings
data Table = Table
  { -- | A list of rows. They must be of the same total length.
    tableCells :: [[[Chunk]]],
    tableColumnSeparator :: Chunk,
    tableBackground :: Maybe TableBackground
  }
  deriving (Show, Eq, Generic)

instance Validity Table

data TableBackground
  = SingleColour Colour
  | Bicolour
      -- Even-numbered table rows (0-indexed)
      (Maybe Colour)
      -- Odd-numbered table rows
      (Maybe Colour)
  deriving (Show, Eq, Generic)

instance Validity TableBackground

-- | Render a table to chunks that can be rendered to text.
renderTable :: Table -> [Chunk]
renderTable = unlinesChunks . renderTableLines

renderTableLines :: Table -> [[Chunk]]
renderTableLines Table {..} =
  let asColumns :: [[[Chunk]]]
      asColumns = transpose (padRows [] tableCells)

      addLengthsToColumn :: [[Chunk]] -> [(Int, [Chunk])]
      addLengthsToColumn = map (\c -> (cellWidth c, c))
      maxLengthOfColum :: [(Int, [Chunk])] -> Int
      maxLengthOfColum = maximum . map fst
      padColumn :: Int -> [(Int, [Chunk])] -> [[Chunk]]
      padColumn maxLength = map (\(l, c) -> c ++ [paddingChunk (maxLength - l) ' '])
      padEntireColumn :: [(Int, [Chunk])] -> [[Chunk]]
      padEntireColumn col =
        let maxLength = maxLengthOfColum col
         in padColumn maxLength col
      paddedColumns :: [[[Chunk]]]
      paddedColumns = map (padEntireColumn . addLengthsToColumn) asColumns
      paddedRows :: [[[Chunk]]]
      paddedRows = transpose paddedColumns

      withBg :: Int -> Chunk -> Chunk
      withBg i = possiblyAddBackground $ backgroundForRow i tableBackground
      renderRow :: Int -> [[Chunk]] -> [Chunk]
      renderRow i = go
        where
          go :: [[Chunk]] -> [Chunk]
          go [] = []
          go [cs] = map (withBg i) cs ++ go []
          go (cs1 : cs2 : rest) =
            map (withBg i) cs1
              ++ [withBg i tableColumnSeparator]
              ++ go (cs2 : rest)
   in iterateLikeInPython renderRow paddedRows

iterateLikeInPython :: (Int -> a -> b) -> [a] -> [b]
iterateLikeInPython f = zipWith f [0 ..]

-- | Make every row contain the same number of cells, irrespective of the
-- width of the cells.
--
-- Use the first argument to fill as the extra cells.
padRows :: forall a. a -> [[a]] -> [[a]]
padRows _ [] = []
padRows d css =
  let withLengths :: [(Int, [a])]
      withLengths = map (\ls -> (length ls, ls)) css
      maximumLength :: Int
      maximumLength = maximum $ map fst withLengths
      pad :: (Int, [a]) -> [a]
      pad (l, cs) = cs ++ replicate (maximumLength - l) d
   in map pad withLengths

cellWidth :: [Chunk] -> Int
cellWidth = sum . map chunkWidth

paddingChunk :: Int -> Char -> Chunk
paddingChunk l c = chunk $ T.pack $ replicate l c

possiblyAddBackground :: Maybe Colour -> Chunk -> Chunk
possiblyAddBackground mb c = c {chunkBackground = chunkBackground c <|> mb}

backgroundForRow :: Int -> Maybe TableBackground -> Maybe Colour
backgroundForRow _ Nothing = Nothing
backgroundForRow _ (Just (SingleColour c)) = Just c
backgroundForRow i (Just (Bicolour ec oc)) = if even i then ec else oc
