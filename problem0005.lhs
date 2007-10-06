> import Data.Maybe
> import Data.List

> merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
>                     | x > y     = y : merge (x:xs) ys
>                     | otherwise = x : merge xs ys
> merge xs     []     = xs
> merge []     ys     = ys
>
> primes = sieve [2..]
>     where sieve (p:rest) = p : sieve [n | n <- rest, mod n p /= 0]
>
> divides m n = n `mod` m == 0
>
> factorise n = unfoldr factor n
>               where factor 1 = Nothing
>                     factor m = do f <- find (`divides` m) (takeWhile (<= m) primes)
>                                   return (f, m `div` f)

> smallest fs = foldl1 (*) factors
>               where factors = foldl1 merge (map factorise fs)

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

> smallest1to10 = smallest [1..10]

What is the smallest number that is evenly divisible by all of the numbers from
1 to 20?

> answer = smallest [1..20]

> main = do print smallest1to10
>           print answer
