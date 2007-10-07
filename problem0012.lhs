> import Data.List

    The sequence of triangle numbers is generated by adding the natural numbers. So
    the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.

Let's define an infinite list of triangle numbers:

> triangles = scanl1 (+) [1..]

    We can see that the 7th triangle number, 28, is the first triangle number
    to have over five divisors.

Ok, we need a way to count the factors of a number:

> countFactors n = sum [count f | f <- takeWhile (\m -> m * m <= n) [1..], n `mod` f == 0]
>                  where count m | m * m == n = 1
>                                | otherwise  = 2

Which is the first triangle number to have over five-hundred divisors?

> answer = find (\n -> countFactors n > 500) triangles
> main = print answer
