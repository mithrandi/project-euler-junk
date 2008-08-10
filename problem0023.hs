import Data.List
import qualified Data.Set as Set
import Data.Array.Unboxed
import Data.Array.ST

import Euler

isAbundant n = sum (divisors n) > n

abundants = filter isAbundant [1..28123]

sums = filter (<= 28123) [x + head xs | xs <- tails abundants, x <- xs]

resultsArray :: UArray Int Int
resultsArray = runSTUArray $ do rs <- newListArray (0, 28123) [0..28123]
                                mapM_ (\x -> writeArray rs (fromInteger x) 0) sums
                                return rs

resultSum = sum . elems $ resultsArray

main = print $ resultSum == 4179871
