import Data.List
import qualified Data.Set as S

import Euler

truncations n = concatMap (map l2i . init . tail) [tails ds, inits ds]
    where ds = digits n

candPrimes :: [Int]
candPrimes = takeWhile (< 1000000) primes

primeSet = S.fromAscList candPrimes

problem0037 = sum . (take 11) $ filter interesting (dropWhile (<10) candPrimes)
    where interesting n = and . (map (`S.member` primeSet)) $ truncations n

main = print problem0037
