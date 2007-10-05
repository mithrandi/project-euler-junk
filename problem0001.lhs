Merge two sorted lists into a single sorted list, discarding duplicates:

> merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
>                     | x > y     = y : merge (x:xs) ys
>                     | otherwise = x : merge xs ys

Multiples of n:

> multiplesOf n = map (*n) [1..]

Multiples of 3 and 5:

> m3and5 = merge (multiplesOf 3) (multiplesOf 5)

If we list all the natural numbers below 10 that are multiples of 3 or 5, we
get 3, 5, 6 and 9. The sum of these multiples is 23.

> below10 = sum $ takeWhile (<10) m3and5

Find the sum of all the multiples of 3 or 5 below 1000.

> below1000 = sum $ takeWhile (<1000) m3and5

> main = do print below10
>           print below1000
