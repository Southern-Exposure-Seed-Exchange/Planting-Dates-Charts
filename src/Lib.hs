module Lib where

import           Parser                         ( parseXlsx )


-- | Parse plants form the given XLSX file & print them out.
app :: FilePath -> IO ()
app xlsxPath = do
    plants <- parseXlsx xlsxPath
    print plants
