import Control.Monad
import Data.List
import Data.Array

import Euler

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- Calculate the proper divisors of an integer
divisors :: Integer -> [Integer]
divisors n = nub $ map product cs
             where cs = tail (powerset . factorise $ n)

-- Let d(n) be defined as the sum of proper divisors of n (numbers less than n
-- which divide evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a
-- and b are an amicable pair and each of a and b are called amicable numbers.
d :: Integer -> Integer
d = sum . divisors

amicable = [n | n <- [2..10000], (d n /= n) && (n == d (d n))]

--main = print $ sum amicable
main = print $ sum amicable
