{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

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
                                                , listToMaybe
                                                , maybeToList
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Read                      ( readMaybe )


-- | Parse plants form the given XLSX file & print them out.
app :: FilePath -> IO ()
app xlsxPath = do
    dates <- parsePlants . toXlsx <$> L.readFile xlsxPath
    print dates


-- | Parse the Plants out of a spreadsheet.
parsePlants :: Xlsx -> [Plant]
parsePlants xlsx =
    let rows = xlsx ^. ixSheet "Sheet1" . wsCells . to toRows
    in  mapMaybe parsePlant rows

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
    maybeTextColumn n =
        listToMaybe (take 1 $ drop n cells) >>= _cellValue . snd >>= \case
            CellText t -> Just t
            _          -> Nothing

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


data Date =
    Date
        { day :: Integer
        , month :: Integer
        }
    deriving (Show)

data DateRange
    = DateRange
        { start :: Date
        , end :: Date
        }
    deriving (Show)

data Plant
    = Plant
        { plant :: Text
        , ranges :: [DateRange]
        }
    deriving (Show)
