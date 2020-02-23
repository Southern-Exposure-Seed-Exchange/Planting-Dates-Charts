{-# LANGUAGE LambdaCase #-}
module Types where

import           Data.Text                      ( Text )


-- | A single point on the calendar.
data Date =
    Date
        { day :: Integer
        , month :: Integer
        }
    deriving (Show, Eq)

instance Ord Date where
    d1 <= d2 =
        month d1 < month d2 || (month d1 == month d2 && day d1 <= day d2)

-- | Calculate the day number for a 'Date', where January 1st is
-- 1 & December 31st is 365.
dayOfYear :: Date -> Integer
dayOfYear date =
    let previousMonths =
                sum $ take (fromIntegral $ month date - 1) daysPerMonth
    in  previousMonths + day date

-- | Get the Date after the passed one.
nextDay :: Date -> Date
nextDay date =
    let isLastDay = day date
            == head (drop (fromIntegral $ month date - 1) daysPerMonth)
    in  if isLastDay
            then Date { day = 1, month = (month date + 1) `mod` 12 }
            else date { day = day date + 1 }

-- | The number of days in each month of the year.
daysPerMonth :: [Integer]
daysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- | Represents the inclusive start & end dates of a range along with the
-- type of range(planting or harvesting).
data DateRange
    = DateRange
        { start :: Date
        , end :: Date
        , rangeType :: RangeType
        }
    deriving (Show, Eq)

instance Ord DateRange where
    r1 <= r2 = start r1 <= start r2

-- If the range wraps around to next year, split it into two ranges.
-- TODO: end dates should never be on first of month - they'll stick out
--       past the month's grid line.
normalize :: DateRange -> [DateRange]
normalize range = if start range > end range
    then
        [ DateRange { start     = Date 1 1
                    , end       = end range
                    , rangeType = rangeType range
                    }
        , DateRange { start     = start range
                    , end       = Date 31 12
                    , rangeType = rangeType range
                    }
        ]
    else [range]

-- | Does one range overlap another?
--
-- Note: You should probably call 'normalize' on the ranges and use
-- 'anyOverlaps' instead of directly calling this function.
overlaps :: DateRange -> DateRange -> Bool
overlaps r1 r2 = isOver r1 r2 || isOver r2 r1
  where
    isOver x y =
        (start y > start x && end y < end x)            -- x surrounds y
            || (start x < start y && end x > start y)   -- x overlaps left edge
            || (start x > start y && start x < end y)   -- x overlaps right edge


-- | Is there any overlap between the two sets of ranges?
anyOverlaps :: [DateRange] -> [DateRange] -> Bool
anyOverlaps xs ys = any (\x -> any (overlaps x) ys) xs


-- | Count the number of days a list of DateRanges contains.
countDays :: [DateRange] -> Integer
countDays = count 0 . concatMap normalize
  where
    count counter = \case
        [] -> counter
        r : rest ->
            count (counter + dayOfYear (end r) - dayOfYear (start r) + 1) rest

-- | Merge DateRanges with the same 'RangeType' if the end date of one is
-- a day before the start date of the next.
--
-- Assumes the list of ranges is already sorted.
mergeAdjacent :: [DateRange] -> [DateRange]
mergeAdjacent = \case
    []  -> []
    [x] -> [x]
    this : next : rest ->
        if rangeType this == rangeType next && nextDay (end this) == start next
            then this { end = end next } : mergeAdjacent rest
            else this : mergeAdjacent (next : rest)

-- | Is the 'DateRange' for Planting or Harvesting?
data RangeType
    = Planting
    | Harvesting
    deriving (Show, Eq)

-- | A RangeBar represents a single row to be rendered in the chart.
data RangeBar
    = SingleBar DateRange
    -- ^ The row contains a single segment
    | MultiBar [DateRange]
    -- ^ The row contains multiple segments
    deriving (Show, Eq)


data Plant
    = Plant
        { plant :: Text
        -- ^ The name of the Plant
        , ranges :: [RangeBar]
        -- ^ The rows of date ranges to render
        }
    deriving (Show)
