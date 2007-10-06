> import Data.List
> import Control.Arrow
> import Data.Array

Define a partially memoised function to count the length of a collatz sequence:

> collatz n | n > ub    = collatz' n
>           | otherwise = listArray (0,ub) (map collatz' [0..]) ! n
>     where collatz' n | n == 0    = 0
>                      | n == 1    = 1
>                      | even n    = 1 + collatz (n `div` 2)
>                      | otherwise = 1 + collatz (3 * n + 1)
>           ub = 500000


Now we scan the first million starting numbers, and find the longest sequence:

> (*) `on` f = \x y -> f x * f y
> answer = fst $ maximumBy (compare `on` snd) $ map (id &&& collatz) [1..999999]
> main = print answer
