> import Data.List

> primes = sieve [2..]
>     where sieve (p:rest) = p : sieve [n | n <- rest, mod n p /= 0]
>
> divides m n = n `mod` m == 0
>
> factorise n = unfoldr factor n
>               where factor 1 = Nothing
>                     factor m = do f <- find (`divides` m) (takeWhile (<= m) primes)
>                                   return (f, m `div` f)

The prime factors of 13195 are 5, 7, 13 and 29.

> factors13195 = factorise 13195

What is the largest prime factor of the number 317584931803?

> largest = last $ factorise 317584931803

> main = do print factors13195
>           print largest
