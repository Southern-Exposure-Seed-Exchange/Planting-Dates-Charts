{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Parser                         ( parseXlsx )
import           Draw                           ( renderPlants )


-- | Parse plants form the given XLSX file & print them out.
app :: FilePath -> Text -> IO ()
app xlsxPath title = do
    plants <- parseXlsx xlsxPath
    let halfLength = ceiling $ fromIntegral (length plants) / (2 :: Double)
        partOne    = take halfLength plants
        partTwo    = drop halfLength plants
    renderPlants (unpack title <> "-part-one.svg") title "(Part One)" partOne
    renderPlants (unpack title <> "-part-two.svg") title "(Part Two)" partTwo
