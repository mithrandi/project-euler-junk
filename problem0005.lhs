> import Data.Maybe
> import Data.List

> import Euler

> smallest fs = foldl1 lcm fs

2520 is the smallest number that can be divided by each of the numbers from 1
to 10 without any remainder.

> smallest1to10 = smallest [1..10]

What is the smallest number that is evenly divisible by all of the numbers from
1 to 20?

> answer = smallest [1..20]

> main = do print smallest1to10
>           print answer
