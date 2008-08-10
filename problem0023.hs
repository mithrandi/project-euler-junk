import Data.List
import qualified Data.Set as Set
import Data.Array.Unboxed
import Data.Array.ST

import Euler

isAbundant n = sum (divisors n) > n

abundants = filter isAbundant [1..28123]

sums = filter (<= 28123) [x + head xs | xs <- tails abundants, x <- xs]

resultsArray = runSTUArray $ do rs <- newListArray (0, 28123::Int) ([0..28123] :: [Int])
                                mapM_ (\x -> writeArray rs (fromInteger x :: Int) 0) sums
                                return rs

--resultSum = sum [1..28123] - Set.fold (+) 0 (Set.fromList sums)
resultSum = sum . elems $ resultsArray

main = print $ resultSum == 4179871
