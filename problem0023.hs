import Data.List
import qualified Data.Set as Set
import Data.Array.Unboxed
import Data.Array.ST

import Euler

limit :: (Num a) => a
limit = 28123

isAbundant :: Integer -> Bool
isAbundant n = sum (divisors n) > n

abundants :: [Integer]
abundants = filter isAbundant [1..limit]

sums :: [Integer]
sums = filter (<= limit) [x + head xs | xs <- tails abundants, x <- xs]

resultsArray :: UArray Int Int
resultsArray = runSTUArray $ do rs <- newListArray (0, limit) [0..limit]
                                mapM_ (\x -> writeArray rs (fromInteger x) 0) sums
                                return rs

resultSum :: Int
resultSum = sum . elems $ resultsArray

main = print resultSum
