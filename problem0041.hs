import Data.List
import qualified Data.Set as S

import Euler

candPrimes :: [Int]
candPrimes = takeWhile (< 31623) primes

isPrime' n = all (not . (`divides` n)) (takeWhile (\p -> p * p <= n) candPrimes)

pandigitals = concatMap (map l2i . permutations) (drop 1 $ inits [1..9])

problem0041 = maximum $ filter isPrime' pandigitals

main = print problem0041
