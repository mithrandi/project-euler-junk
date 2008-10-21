import Euler
import Data.List
import Data.Function

quad a b n = (n * n) + (a * n) + b

countPrimes a b = length . (takeWhile isPrime) $ map (quad a b) [0..]

problem0027 = fst $ maximumBy (compare `on` snd) [(a * b, countPrimes a b) | a <- [-999..999], b <- [-999..999]]

main = print problem0027
