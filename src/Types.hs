module Types where

import           Data.Text                      ( Text )


data Date =
    Date
        { day :: Integer
        , month :: Integer
        }
    deriving (Show)

data DateRange
    = DateRange
        { start :: Date
        , end :: Date
        }
    deriving (Show)

data Plant
    = Plant
        { plant :: Text
        , ranges :: [DateRange]
        }
    deriving (Show)
