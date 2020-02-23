{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Draw
    ( renderPlants
    )
where

import           Data.List                      ( intersperse
                                                , sort
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Diagrams.Prelude        hiding ( start
                                                , end
                                                , orange
                                                , normalize
                                                , grey
                                                )
import           Diagrams.Backend.SVG           ( B
                                                , renderSVG
                                                )
import           Diagrams.Color.XKCD            ( brownyOrange
                                                , slateGrey
                                                , orange
                                                , emerald
                                                , grey
                                                , darkGrey
                                                )
import           Graphics.SVGFonts              ( textSVG_
                                                , TextOpts(..)
                                                )
import           Graphics.SVGFonts.ReadFont     ( PreparedFont
                                                , loadFont
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Paths_planting_dates           ( getDataFileName )
import           Types


-- Styles
-- TODO: It'd be cool if *all* the styling constants were pulled out of the
-- rendering functions into a Style datatype that we passed via a Reader to
-- the rendering functions. Then we could take a YAML config for custom
-- styles & offer some CLI arguments to tweak them for individual runs.

-- | The Height of the Date Range Boxes.
boxHeight :: Num a => a
boxHeight = 5

-- | The Space Between Each Row & the Dividers.
rowPadding :: Num a => a
rowPadding = 3

-- | The Height of the Axis Label's Text
labelTextHeight :: Double
labelTextHeight = (boxHeight + 2 * rowPadding) * 0.66

greyscale :: Bool
greyscale = False

-- | The Fill Color of the Harvesting Range Boxes
harvestingColor :: AlphaColour Double
harvestingColor = if greyscale then darkGrey else emerald

-- | The Fill Color of the Planting Range Boxes
plantingColor :: AlphaColour Double
plantingColor = if greyscale then grey else orange

rangeLineWidth :: Fractional a => a
rangeLineWidth = 0.2

-- | Opacity of the Row Separators Between Plants.
plantSeparatorOpacity :: Double
plantSeparatorOpacity = 0.5

-- | Opacity of the Row Separators Between a Plant's Bars.
barSeparatorOpacity :: Double
barSeparatorOpacity = 0.35

-- | Opacity of the Vertical Month Grid Lines.
monthLineOpacity :: Double
monthLineOpacity = 0.65

-- | Render all the plants to the given SVG file.
renderPlants :: FilePath -> Text -> Text -> [Plant] -> IO ()
renderPlants outputFile title subtitle ps =
    let
        size_        = mkSizeSpec2D Nothing Nothing
        plantRows    = map renderPlant ps
        rowSeparator = renderRowSep $ maximum_ $ map width plantRows
        chartRows =
            rowSeparator : intersperse rowSeparator plantRows ++ [rowSeparator]
        finalChart =
            chartRows
                # vsep rowPadding
                # withGrid
                # withLegend
                # withTitle title subtitle
                # centerXY
                # pad 1.1
                # withBackground 0.75
    in
        renderSVG outputFile size_ finalChart


-- Page Elements

-- | Place a white background with a black stroke behind the diagram
withBackground :: Measured Double Double -> Diagram B -> Diagram B
withBackground strokeWidth diagram =
    diagram
        #  centerXY
        <> rect (width diagram) (height diagram)
        #  fc white
        #  lw strokeWidth

-- | Place a Header above the diagram.
withTitle :: Text -> Text -> Diagram B -> Diagram B
withTitle title subtitle diagram =
    vcat $ map centerX [renderHeader title subtitle # centerX, diagram]

-- | Render the Header with a Title & SubTitle.
renderHeader :: Text -> Text -> Diagram B
renderHeader title subtitle =
    let topHeader =
                headerText (labelTextHeight * 2.5) title # fc black # centerX
        subHeader =
                headerText (labelTextHeight * 1.25) subtitle
                    # fcA slateGrey
                    # centerX
    in  vcat
            [ topHeader
            , strutY (rowPadding * 0.35)
            , subHeader
            , strutY (rowPadding * 1.75)
            ]
    where headerText n t = svgText n t futuraHeavy

-- | Render a Legend below the Diagram.
withLegend :: Diagram B -> Diagram B
withLegend diagram = vcat $ map
    alignL
    [ diagram
    , strutY 5
    , vcat
        (map
            alignL
            [ legendLabel plantingColor "Planting"
            , strutY 2
            , legendLabel harvestingColor "Harvesting"
            ]
        )
    # centerXY
    # pad 1.5
    # withBackground 0.33
    ]
  where
    legendLabel :: AlphaColour Double -> Text -> Diagram B
    legendLabel color label = hcat $ map
        centerY
        [ rect 10 boxHeight # fcA color # lc black # lw 0.25
        , strutX 2
        , svgText (labelTextHeight * 0.85) label futuraMedium # fc black
        ]


-- GRID

-- | Place the Month Labels & Grid Behind the Diagram.
withGrid :: Diagram B -> Diagram B
withGrid diagram =
    alignTR
            (  alignTR renderMonthLabels
            <> alignBR
                   (  alignR diagram
                   <> renderGrid (width diagram) (height diagram)
                   )
            )
        <> alignBR renderMonthLabels

-- | Render the X-axis Grid Lines
renderGrid :: Double -> Double -> Diagram B
renderGrid w h =
    let baseLine =
                fromVertices [p2 (0, 0), p2 (w, 0)]
                    # strokeP
                    # lw 0.45
                    # lc brown
                    # alignR
        monthLines = alignTR $ renderMonthLines h
    in  baseLine === monthLines === baseLine

-- | Render the Month Labels & Separators
renderMonthLabels :: Diagram B
renderMonthLabels =
    let
        labels      = foldr buildLabel mempty $ zip daysPerMonth monthLabels
        labelHeight = height labels
        monthLines  = alignTR $ renderMonthLines $ labelHeight + 2 * rowPadding
    in
        monthLines
            <> alignTR (strutY rowPadding === labels === strutY rowPadding)
  where
      -- | Build and prepend a label & invisible bounding box.
    buildLabel :: (Integer, Text) -> Diagram B -> Diagram B
    buildLabel (w, t) acc =
        let label = renderText t
            textBox =
                    centerXY (rect (fromIntegral w) (height label) # lw none)
                        <> centerXY label
        in  alignR textBox ||| alignL acc


-- | Render the vertical Month lines, aligned to the right.
renderMonthLines :: Double -> Diagram B
renderMonthLines h =
    let verticalLine =
                fromVertices [p2 (0, 0), p2 (0, h)]
                    # strokeP
                    # lw 0.35
                    # lc brown
                    # opacity monthLineOpacity
                    # dashing [1, 1] 0
    in
        hcat
            (map
                (\w_ -> alignR $ verticalLine ||| strutX
                    (fromIntegral w_ - width verticalLine)
                )
                daysPerMonth
            )

-- | Draw a row separator.
renderRowSep :: Double -> Diagram B
renderRowSep w =
    strokeP (fromVertices [p2 (0, 0), p2 (w, 0)])
        # lw 0.2
        # alignR
        # opacity plantSeparatorOpacity
        # lcA brownyOrange

-- | Draw a Plant's row.
renderPlant :: Plant -> Diagram B
renderPlant p = alignR $ hcat $ map
    centerY
    [renderPlantLabel (plant p), strutX 5, renderBars (ranges p)]


-- | Render a right-aligned label for the plant.
renderPlantLabel :: Text -> Diagram B
renderPlantLabel = renderText

-- | Render text as an SVG path with the given height & font. The text will
-- be transparent, you still need to set the fill colour yourself.
--
-- Note: Uses unsafePerformIO! We should instead load the fonts in the Main
-- module & pass them around so we only have to load them once.
--
-- TODO: Take a color as an arg so we can't accidentally add transparent
-- text?
svgText :: Double -> Text -> IO (PreparedFont Double) -> Diagram B
svgText h t f =
    textSVG_ with { textHeight = h, textFont = unsafePerformIO f } (unpack t)
        # lw none

-- | Render the RangeBar rows for a Plant. Plants with multiple rows will
-- have light row separators between them.
renderBars :: [RangeBar] -> Diagram B
renderBars = vcat . map alignR . intersperse barRowSeperator . map renderBarRow
  where
    barRowSeperator = vcat
        [strutY 1, renderRowSep 365 # opacity barSeparatorOpacity, strutY 1]
    renderBarRow = \case
        SingleBar r -> renderRanges $ normalize r
        MultiBar rs ->
            renderRanges $ mergeAdjacent $ sort $ concatMap normalize rs


-- | Render Date Ranges By Interspersing Them with Empty Rectangles.
renderRanges :: [DateRange] -> Diagram B
renderRanges ranges_ =
    let (diagram, lastDay) = foldl renderEmptyAndRange (mempty, 0) ranges_
    in  if lastDay == 365
            then diagram
            else diagram ||| emptyRangeBox (365 - lastDay)
  where
    -- Render the previous empty range and the current DateRange.
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
        color      = case rangeType of
            Planting   -> plantingColor
            Harvesting -> harvestingColor
    in  rect (fromIntegral $ dayOfEnd - dayOfStart + 1) boxHeight
            # lw rangeLineWidth
            # fcA color

-- | Draw a transparent rectangle with no border.
emptyRangeBox :: Integer -> Diagram B
emptyRangeBox w = rect (fromIntegral w) boxHeight # lw 0


-- UTILS

-- | Render some black text in Futura Medium.
renderText :: Text -> Diagram B
renderText t = svgText labelTextHeight t futuraMedium # fc black

-- | The labels for each month.
monthLabels :: [Text]
monthLabels =
    [ "Jan"
    , "Feb"
    , "Mar"
    , "Apr"
    , "May"
    , "Jun"
    , "Jul"
    , "Aug"
    , "Sep"
    , "Oct"
    , "Nov"
    , "Dec"
    ]

-- | Determine the maximum number or return 0 for the empty list.
maximum_ :: (Num a, Ord a) => [a] -> a
maximum_ xs = if null xs then 0 else maximum xs

-- | Load the customized FuturaBT Heavy font. This has many symbols
-- stripped out to imporve the load time.
futuraHeavy :: IO (PreparedFont Double)
futuraHeavy = getDataFileName "data/FuturaBT-Heavy.svg" >>= loadFont

-- | Load the customized FuturaBT Medium font. This has many symbols
-- stripped out to imporve the load time.
futuraMedium :: IO (PreparedFont Double)
futuraMedium = getDataFileName "data/FuturaBT-Medium.svg" >>= loadFont
