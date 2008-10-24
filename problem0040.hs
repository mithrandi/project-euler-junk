import Data.Char

fraction = concatMap show [0..]

problem0040 = product . map (digitToInt) . map (fraction !!) $ [1, 10, 100, 1000, 10000, 100000, 1000000]

main = print problem0040
