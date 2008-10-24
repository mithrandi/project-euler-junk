import Data.List

import Euler

prods n = dropWhile (\s -> length s < 9) . takeWhile (\s -> length s <= 9) . (map cat') . tail . inits $ map (*n) [1..]
    where cat' xs = concatMap show xs

problem0038 = maximum . (filter pandigital) . (concatMap prods) $ [1..100000]
    where pandigital s = (sort s) == ['1'..'9']

main = print problem0038
