import Data.List
import Test.QuickCheck
import qualified Data.Set as Set

import Euler

isAbundant :: Integer -> Bool
isAbundant n = sum (divisors n) > n

abundants :: [Integer]
abundants = filter isAbundant [1..28123]

sums :: Set.Set Integer
sums = Set.fromList . filter (<= 28123) $ [x + head xs | xs <- tails abundants, x <- xs]

results :: Set.Set Integer
results = Set.difference (Set.fromList [1..28123]) sums

resultSum :: Integer
resultSum = Set.fold (+) 0 results

main = print resultSum
