module Euler where

import Data.List (unfoldr, find, nub, inits, tails)
import Data.Char (digitToInt)
import Control.Monad (filterM)
import Control.Applicative ((<$>), (<*>))

merge :: (Ord t) => [t] -> [t] -> [t]
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys
merge xs     []     = xs
merge []     ys     = ys

primes :: (Integral a) => [a]
primes = 2 : filter isPrime [3, 5..]
    where isPrime n = all (not . (`divides` n)) (takeWhile (\p -> p * p <= n) primes)

isPrime :: Integer -> Bool
isPrime n = n `elem` (takeWhile (<= n) primes)

divides :: (Integral a) => a -> a -> Bool
divides m n = n `mod` m == 0

factorise :: (Integral a) => a -> [a]
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

digits :: (Num n) => n -> [Int]
digits n = map digitToInt (show n)

choices :: [a] -> [(a, [a])]
choices = map (\(x,y) -> (head y, x ++ tail y)) . init . (zip <$> inits <*> tails)

choose :: Int -> [a] -> (a, [a])
choose n xs = let (l, x:r) = splitAt n xs
              in (x, l ++ r)

permutations :: [a] -> [[a]]
permutations []   = [[]]
permutations xs   = do (x, rest) <- choices xs
                       map (x:) (permutations rest)
