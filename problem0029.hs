import Data.List

problem29 n = length . nub $ [a ^ b | a <- [2..n], b <- [2..n]]

main = print (problem29 100)
