import Euler
import Control.Applicative
import Data.List
import qualified Data.Set as S

rotations :: Int -> [Int]
rotations n = map l2i $ zipWith (++) <$> tails <*> inits $ digits n

candPrimes :: [Int]
candPrimes = takeWhile (< 1000000) primes

primeSet = S.fromAscList candPrimes

isCircular n = and . (map (`S.member` primeSet)) $ rotations n

problem0035 = length $ filter isCircular candPrimes

main = print problem0035
