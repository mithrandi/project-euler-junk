> import Data.List
> import Euler

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

> prime6 = primes !! (6 - 1)

What is the 10001st prime number?

> answer = primes !! (10001 - 1)

> main = do print prime6
>           print answer
