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

resultSum :: Integer
resultSum = sum [1..28123] - Set.fold (+) 0 sums

main = print resultSum
