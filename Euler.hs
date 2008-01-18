module Euler where

import Data.List (unfoldr, find)

merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys
merge xs     []     = xs
merge []     ys     = ys

primes = sieve [2..]
    where sieve (p:rest) = p : sieve [n | n <- rest, mod n p /= 0]

divides m n = n `mod` m == 0

factorise n = unfoldr factor n
              where factor 1 = Nothing
                    factor m = do f <- find (`divides` m) (takeWhile (<= m) primes)
                                  return (f, m `div` f)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p xs = case dropWhile p xs of
    [] -> []
    xs' -> x : splitBy p xs'' where (x, xs'') = break p xs'

