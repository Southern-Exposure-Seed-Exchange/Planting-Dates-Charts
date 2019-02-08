module Lib where

import           Parser                         ( parseXlsx )
import           Draw                           ( renderPlants )


-- | Parse plants form the given XLSX file & print them out.
app :: FilePath -> IO ()
app xlsxPath = do
    plants <- parseXlsx xlsxPath
    let halfLength = ceiling $ fromIntegral (length plants) / (2 :: Double)
        partOne    = take halfLength plants
        partTwo    = drop halfLength plants
    renderPlants "plants-chart-part-one.svg" partOne
    renderPlants "plants-chart-part-two.svg" partTwo
