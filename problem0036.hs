import Euler

problem0036 = sum $ filter (\n -> palindromicBase 2 n && palindromicBase 10 n) [1..1000000]

main = print problem0036
