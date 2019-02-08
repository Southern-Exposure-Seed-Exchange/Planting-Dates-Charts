{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Draw where

import           Data.List                      ( intersperse )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Diagrams.Prelude        hiding ( start
                                                , end
                                                )
import           Diagrams.Backend.SVG           ( B
                                                , renderSVG
                                                )
import           Diagrams.Color.XKCD            ( brownyOrange )
import           Graphics.SVGFonts              ( textSVG'
                                                , TextOpts(..)
                                                , lin2
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Types

-- | Render all the plants to the given SVG file.
renderPlants :: FilePath -> [Plant] -> IO ()
renderPlants outputFile ps = do
    let size_          = mkSizeSpec2D Nothing Nothing
        barsWithLabels = map renderPlant ps
        maximumWidth   = maximum_ $ map width barsWithLabels
    renderSVG outputFile size_ $ pad 1.1 $ centerXY $ vsep 5 $ intersperse
        (renderRowSep maximumWidth)
        barsWithLabels
    where maximum_ xs = if null xs then 0 else maximum xs


-- | The Height of the Date Range Boxes.
boxHeight :: Num a => a
boxHeight = 5

-- | The Space Between Each Row & the Dividers.
rowPadding :: Num a => a
rowPadding = 5

-- | Render the X-axis Scale & Grid Lines
renderGrid :: Double -> Double -> Diagram B
renderGrid w h =
    let
        baseLine =
            fromVertices [p2 (0, 0), p2 (w, 0)] # strokeP # lw 0.45 # lc brown
        verticalLine =
            fromVertices [p2 (0, 0), p2 (0, h)]
                # strokeP
                # lw 0.35
                # lc brown
                # opacity 0.6
                # dashing [1, 1] 0
        monthLines = verticalLine ||| hcat
            (map (\w_ -> alignR $ strutX (fromIntegral w_) ||| verticalLine)
                 daysPerMonth
            )
    in
        alignBR baseLine <> alignBR monthLines

-- | Draw a row seperator.
renderRowSep :: Double -> Diagram B
renderRowSep w =
    strokeP (fromVertices [p2 (0, 0), p2 (w, 0)])
        # lw 0.2
        # alignR
        # opacity 0.25
        # lcA brownyOrange

-- | Draw a Plant's row.
renderPlant :: Plant -> Diagram B
renderPlant p =
    alignR $ renderPlantLabel (plant p) ||| strutX 5 ||| renderDateRanges
        (ranges p)


-- | Render a right-aligned label for the plant.
renderPlantLabel :: Text -> Diagram B
renderPlantLabel = alignR . renderText

renderText :: Text -> Diagram B
renderText t =
    textSVG'
            with { textHeight = boxHeight * 1.25
                 , textFont   = unsafePerformIO lin2
                 }
            (unpack t)
        # stroke
        # fc black
        # lw none
        # alignR

-- | Draw Bars for Date Ranges by joining empty and filled rectangles
-- horiztonally.
renderDateRanges :: [DateRange] -> Diagram B
renderDateRanges ranges_ = case ranges_ of
    []            -> mempty
    [singleRange] -> renderSingleDateRange singleRange
    rs            -> renderMultipleRanges rs


-- | Render a Single Date Range by Adding Empty Before/After Rectangles
renderSingleDateRange :: DateRange -> Diagram B
renderSingleDateRange range =
    let dayOfStart  = dayOfYear $ start range
        dayOfEnd    = dayOfYear $ end range
        beforeRange = if dayOfStart /= 1
            then emptyRangeBox (fromIntegral dayOfStart - 1)
            else mempty
        afterRange = if dayOfEnd /= 365
            then emptyRangeBox (365 - fromIntegral dayOfEnd)
            else mempty
    in  beforeRange ||| rangeBox range ||| afterRange

-- | Render Multiple Date Ranges By Interspersing Them with Empty
-- Rectangles.
renderMultipleRanges :: [DateRange] -> Diagram B
renderMultipleRanges ranges_ =
    let (diagram, lastDay) = foldl renderEmptyAndRange (mempty, 0) ranges_
    in  if lastDay == 365
            then diagram
            else diagram ||| emptyRangeBox (365 - lastDay)
  where
      -- | Render the previous empty range and the current DateRange.
    renderEmptyAndRange
        :: (Diagram B, Integer) -> DateRange -> (Diagram B, Integer)
    renderEmptyAndRange (acc, lastRangeEnd) range =
        let dayOfStart = dayOfYear $ start range
            dayOfEnd   = dayOfYear $ end range
            filledBox  = rangeBox range
        in  (, dayOfEnd) $ if dayOfStart == lastRangeEnd + 1
                then acc ||| filledBox
                else
                    acc
                    ||| emptyRangeBox
                            (fromIntegral $ dayOfStart - lastRangeEnd - 1)
                    ||| filledBox

-- | Draw a filled rectangle with the appropriate width for a DateRange.
rangeBox :: DateRange -> Diagram B
rangeBox DateRange {..} =
    let dayOfStart = dayOfYear start
        dayOfEnd   = dayOfYear end
    in  rect (fromIntegral $ dayOfEnd - dayOfStart + 1) boxHeight
            # lw 0.2
            # fc orange

-- | Draw a transparent rectangle with no border.
emptyRangeBox :: Integer -> Diagram B
emptyRangeBox w = rect (fromIntegral w) boxHeight # lw 0



-- Calculate the day number for a 'Date', where January 1st is 1 & December
-- 31st is 365.
dayOfYear :: Date -> Integer
dayOfYear date =
    let previousMonths =
                sum $ take (fromIntegral $ month date - 1) daysPerMonth
    in  previousMonths + day date


-- | The number of days in each month of the year.
daysPerMonth :: [Integer]
daysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
