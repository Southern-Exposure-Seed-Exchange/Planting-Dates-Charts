module Lib where

import           Parser                         ( parseXlsx )
import           Draw                           ( renderPlants )


-- | Parse plants form the given XLSX file & print them out.
app :: FilePath -> IO ()
app xlsxPath = do
    plants <- parseXlsx xlsxPath
    renderPlants "plants-chart.svg" plants
