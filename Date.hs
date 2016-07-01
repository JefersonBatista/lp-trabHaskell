module Date
( Date(..)              -- Construtor de data
, stringToDate          -- Converte uma string em uma data
, strDate               -- Converte uma data em uma string
, compareDate           -- Compara duas datas
) where

import Data.List

data Date = Date
    { day :: Int
    , month :: Int
    , year :: Int
    }
    
stringToDate str = Date
    { day = read $ takeWhile (/= '/') str
    , month = read $ takeWhile (/= '/') rest1
    , year = read rest2
    }
    where
        rest1 = tail $ dropWhile (/= '/') str
        rest2 = tail $ dropWhile (/= '/') rest1
        
strDate date = (print $ day date) ++ "/" ++ (print $ month date) ++ "/" ++ (show $ year date)
    where print n = if n < 10
                    then '0':(show n)
                    else show n

compareDate :: Date -> Date -> Ordering        
compareDate d1 d2
    | year  d1 > year  d2 = GT
    | year  d1 < year  d2 = LT
    | month d1 > month d2 = GT
    | month d1 < month d2 = LT
    | day   d1 > day   d2 = GT
    | day   d1 < day   d2 = LT
    | otherwise = EQ

