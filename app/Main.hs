{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Lib                            ( app )

main :: IO ()
main = do
    app "ZONES 5-6.xlsx" "Planting & Harvest Dates - Zone 5 & 6"
    app "ZONES 7-8.xlsx" "Planting & Harvest Dates - Zone 7 & 8"
