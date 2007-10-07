import Data.List
import Control.Monad
import Control.Monad.Instances

year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapYear = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

divides m n = n `mod` m == 0

isLeapYear y | 100 `divides` y = 400 `divides` y
             | otherwise       = 4 `divides` y

century = do y <- [1900..2000]
             if isLeapYear y then leapYear else year

monthStarts = snd $ mapAccumL (\d m -> join (,) ((d + m) `mod` 7)) 1 century
answer = length . (filter (== 0)) $ applicable
    where applicable = init $ drop 12 monthStarts
main = print answer
