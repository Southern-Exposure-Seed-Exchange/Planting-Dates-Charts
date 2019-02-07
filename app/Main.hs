{-# LANGUAGE LambdaCase #-}
module Main where

import           Lib                            ( app )
import           System.Environment             ( getArgs )

main :: IO ()
main = getArgs >>= \case
    [fileName] -> app fileName
    a -> error $ "planting-dates: Expected 1 argument, got " ++ show (length a)
