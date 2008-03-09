> import Data.List

> divides m n = n `mod` m == 0

> primes = 2 : filter isPrime [3, 5..]
>              where isPrime n = all (not . (`divides` n)) (takeWhile (\p -> p * p <= n) primes)

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

> primesBelow10 = sum $ takeWhile (<10) primes

Find the sum of all the primes below two million.

> answer = sum $ takeWhile (<2000000) primes

> main = do print primesBelow10
>           print answer
