module Euler where

import Data.List (unfoldr, find, nub)
import Control.Monad (filterM)

merge :: (Ord t) => [t] -> [t] -> [t]
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys
merge xs     []     = xs
merge []     ys     = ys

primes :: [Integer]
primes = sieve [2..]
    where sieve (p:rest) = p : sieve [n | n <- rest, mod n p /= 0]

isPrime :: Integer -> Bool
isPrime n = n `elem` (takeWhile (<= n) primes)

divides :: (Integral a) => a -> a -> Bool
divides m n = n `mod` m == 0

factorise :: Integer -> [Integer]
factorise n = unfoldr factor n
              where factor 1 = Nothing
                    factor m = do f <- find (`divides` m) (takeWhile (<= m) primes)
                                  return (f, m `div` f)

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- Calculate the proper divisors of an integer
divisors :: Integer -> [Integer]
divisors n = nub $ map product cs
             where cs = tail (powerset . factorise $ n)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case dropWhile p xs of
    [] -> []
    xs' -> x : splitBy p xs'' where (x, xs'') = break p xs'
