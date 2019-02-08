{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser where

import           Codec.Xlsx                     ( Xlsx
                                                , Cell(_cellValue)
                                                , CellValue(CellText)
                                                , toXlsx
                                                , ixSheet
                                                , wsCells
                                                , toRows
                                                )
import           Control.Lens                   ( (^.)
                                                , to
                                                )
import           Control.Monad                  ( join )
import qualified Data.ByteString.Lazy          as L
import           Data.Maybe                     ( mapMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Read                      ( readMaybe )
import           Types

parseXlsx :: FilePath -> IO [Plant]
parseXlsx xlsxPath = parsePlants . toXlsx <$> L.readFile xlsxPath

-- | Parse the Plants out of a spreadsheet, ignoring the header row
parsePlants :: Xlsx -> [Plant]
parsePlants xlsx =
    let rows = xlsx ^. ixSheet "Sheet1" . wsCells . to toRows
    in  mapMaybe parsePlant $ drop 1 rows

-- | Parse a Plant & It's Planting Date.
--
-- Rows with no Plant Name or no Planting Ranges are ignored.
parsePlant :: (Int, [(Int, Cell)]) -> Maybe Plant
parsePlant (_, cells) =
    let name          = maybeTextColumn 0
        plantingRange = rangeFromCells 1 2
        fallRange     = rangeFromCells 5 6
        ranges_       = maybeToList plantingRange ++ maybeToList fallRange
    in  if null ranges_ then Nothing else Plant <$> name <*> pure ranges_
  where
    rangeFromCells :: Int -> Int -> Maybe DateRange
    rangeFromCells x y =
        join $ parseDateRange <$> maybeTextColumn x <*> maybeTextColumn y
    maybeTextColumn :: Int -> Maybe Text
    maybeTextColumn n = runner cells >>= \case
        CellText t -> Just t
        _          -> Nothing
      where
        runner cs = case cs of
            (i, v) : rest -> if i - 1 == n then _cellValue v else runner rest
            []            -> Nothing

-- | Parse a Start & End Date.
parseDateRange :: Text -> Text -> Maybe DateRange
parseDateRange startDate endDate =
    DateRange <$> parseDate startDate <*> parseDate endDate

-- | Parse a Date in MM/DD Format.
parseDate :: Text -> Maybe Date
parseDate t = case T.split (== '/') t of
    [month_, day_] ->
        Date <$> readMaybe (T.unpack day_) <*> readMaybe (T.unpack month_)
    _ -> Nothing
