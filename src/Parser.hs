{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Parser
    ( parseXlsx
    )
where

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
import           Data.Char                      ( isAlpha
                                                , toUpper
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Read                      ( readMaybe )
import           Types                          ( Plant(..)
                                                , Date(..)
                                                , DateRange(..)
                                                , RangeType(..)
                                                , RangeBar(..)
                                                , countDays
                                                , normalize
                                                , anyOverlaps
                                                )

-- | Parse an xlsx file at the given path.
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
    let
        name          = upperCase . T.toLower <$> maybeTextColumn 0
        springRange   = rangeFromCells Planting 1 2
        springHarvest = rangeFromCells Harvesting 3 4
        fallRange     = rangeFromCells Planting 5 6
        fallHarvest   = rangeFromCells Harvesting 7 8
        bars = mergeRanges (springRange, springHarvest, fallRange, fallHarvest)
    in
        if null bars then Nothing else Plant <$> name <*> pure bars
  where
    rangeFromCells :: RangeType -> Int -> Int -> Maybe DateRange
    rangeFromCells rt x y =
        join
            $   parseDateRange
            <$> pure rt
            <*> maybeTextColumn x
            <*> maybeTextColumn y
    maybeTextColumn :: Int -> Maybe Text
    maybeTextColumn n = runner cells >>= \case
        CellText t -> Just t
        _          -> Nothing
      where
        runner cs = case cs of
            (i, v) : rest -> if i - 1 == n then _cellValue v else runner rest
            []            -> Nothing
    upperCase :: Text -> Text
    upperCase t =
        T.unwords . map (fst . T.foldl upperFirst ("", False)) $ T.words t
      where
        upperFirst (acc, doneProcessing) l
            | doneProcessing = (acc <> T.singleton l, True)
            | isAlpha l      = (acc <> T.singleton (toUpper l), True)
            | otherwise      = (acc <> T.singleton l, False)

-- | Build the list of RangeBars for a Plant using the Spreadsheet's Date
-- Ranges.
--
-- For plants with only spring ranges, we simply merge them if there's no
-- overlaps & split them if there is.
--
-- For plants with spring ranges & only a fall planting range(no fall
-- harvest range), or vice versa - we join the planting ranges if there's
-- no overlap, or the spring ranges if ther's no overlap, falling back to
-- split bars for all ranges.
--
--
-- The other supported format is plants with both spring & fall ranges.
--
-- Harvest ranges are merged if they take up more than 2/3rds of a year.
-- In this case, planting dates will be merged if they don't overlap.
--
-- If there's no overlap in each season's planting & harvest dates, each
-- season will be a single bar. Otherwise we split out the overlapping
-- season into two bars.
--
-- If there's overlap in both seasons, but no overlap in the planting
-- & harvest ranges, we split into a bar for each RagneType. If the
-- planting dates overlap, we'll split those out into separate bars.
--
-- Finally, if nothing has matched yet, we give up and just give each range
-- it's own bar.
mergeRanges
    :: (Maybe DateRange, Maybe DateRange, Maybe DateRange, Maybe DateRange)
    -> [RangeBar]
mergeRanges = \case
    (Just p, Just h, Nothing, Nothing) -> if p `normalizedOverlap` h
        then [SingleBar p, SingleBar h]
        else [MultiBar [p, h]]
    (Just sP, Just sH, Just fP, Nothing)
        | not (sP `normalizedOverlap` fP) -> [MultiBar [sP, fP], SingleBar sH]
        | not (sP `normalizedOverlap` sH) -> [MultiBar [sP, sH], SingleBar fP]
        | otherwise -> map SingleBar [sP, fP, sH]
    (Just sP, Nothing, Just fP, Just fH)
        | not (sP `normalizedOverlap` fP) -> [MultiBar [sP, fP], SingleBar fH]
        | not (fP `normalizedOverlap` fH) -> [SingleBar sP, MultiBar [fP, fH]]
        | otherwise -> map SingleBar [sP, fP, fH]
    (Just sP, Just sH, Just fP, Just fH)
        | countDays [sH, fH] > ceiling (365 * 0.66 :: Double) && not
            (sP `normalizedOverlap` fP)
        -> -- Large Harvest, No Planting Overlap
           [MultiBar [sP, fP], MultiBar [sH, fH]]
        | countDays [sH, fH] > ceiling (365 * 0.66 :: Double)
        -> -- Large Harvest, Planting Overlap
           [SingleBar sP, SingleBar fP, MultiBar [sH, fH]]
        | not (sP `normalizedOverlap` sH) && not (fP `normalizedOverlap` fH)
        -> -- No Overlap By Season
           [MultiBar [sP, sH], MultiBar [fP, fH]]
        | not (sP `normalizedOverlap` sH)
        -> -- Overlap in Fall
           [MultiBar [sP, sH], SingleBar fP, SingleBar fH]
        | not (fP `normalizedOverlap` fH)
        -> -- Overlap in Spring
           [SingleBar sP, SingleBar sH, MultiBar [fP, fH]]
        | ([sP, fP] `normalizedAnyOverlap` [sH, fH])
            && not (sP `normalizedOverlap` fP)
            && not (sH `normalizedOverlap` fH)
        -> -- Planting Overlaps Harvest, No Overlap in RangeType
           [MultiBar [sP, fP], MultiBar [sH, fH]]
        | [sP, fP] `normalizedAnyOverlap` [sH, fH] && not
            (sH `normalizedOverlap` fH)
        -> -- Planting Overlaps Harvest, No Harvest Overlap
           [SingleBar sP, SingleBar fP, MultiBar [sH, fH]]
        | otherwise
        -> -- Give each range it's own bar.
           map SingleBar [sP, sH, fP, fH]
    e -> error $ "unexpected case: " ++ show e
  where
    normalizedOverlap :: DateRange -> DateRange -> Bool
    normalizedOverlap x y = normalize x `anyOverlaps` normalize y
    normalizedAnyOverlap :: [DateRange] -> [DateRange] -> Bool
    normalizedAnyOverlap xs ys =
        concatMap normalize xs `anyOverlaps` concatMap normalize ys

-- | Parse a Start & End Date.
parseDateRange :: RangeType -> Text -> Text -> Maybe DateRange
parseDateRange dateType startDate endDate =
    DateRange <$> parseDate startDate <*> parseDate endDate <*> pure dateType

-- | Parse a Date in MM/DD Format.
parseDate :: Text -> Maybe Date
parseDate t = case T.split (== '/') t of
    [month_, day_] ->
        Date <$> readMaybe (T.unpack day_) <*> readMaybe (T.unpack month_)
    _ -> Nothing
