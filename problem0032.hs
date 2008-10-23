import Euler
import Data.List
import Data.Function

l2i :: [Integer] -> Integer
l2i = foldl' (\z x -> x + z * 10) 0

splitTwice x y s = (a,b,c)
  where (a,p1) = splitAt x s
        (b,c) = splitAt y p1


products = map (\(_,_,p) -> p) $ filter (\(a,b,c) -> a * b == c) candidates
  where
    candidates = map (\(a,b,c) -> (l2i a, l2i b, l2i c)) candidates'
    candidates' = [splitTwice x y p | p <- perms, x <- [1..7], y <- [1..8 - x]]
    perms = permutations [1..9]

problem0032 = sum . nub $ products

main = print problem0032
