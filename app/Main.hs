{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                            ( app )

main :: IO ()
main = do
    app "ZONES 5-6.xlsx" "Zones 5 & 6 Planting Dates"
    app "ZONES 7-8.xlsx" "Zones 7 & 8 Planting Dates"
