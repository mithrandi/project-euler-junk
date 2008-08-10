import Data.List
import Test.QuickCheck

import Euler

isAbundant :: Integer -> Bool
isAbundant n = sum (divisors n) > n

abundants :: [Integer]
abundants = filter isAbundant [1..28123]

diff :: (Ord a) => [a] -> [a] -> [a]
diff []     _               = []
diff xs     []              = xs
diff (x:xs) (y:ys) | x == y = diff xs ys
                   | x < y  = x : diff xs (y:ys)
                   | x > y  = diff (x:xs) ys

prop_Univ u = diff u [] == u
prop_Null s = diff [] s == []
prop_Diff :: [Integer] -> [Integer] -> Bool
prop_Diff u s = all (`notElem` s') (diff u' s')
                where s' = nub (sort s)
                      u' = nub (sort u)

sums = diagSum abundants

diagSum :: (Num a, Ord a) => [a] -> [a]
diagSum xs = nub $ diag' xs xs []
             where diag' []     b      []  = []
                   diag' []     []     acc = []
                   diag' []     (b:bs) acc = sort (zipWith (+) acc' bs) ++ diag' [] bs acc'
                                             where acc' = reverse $ sort acc
                   diag' (a:as) bs     acc = sort (zipWith (+) acc' bs) ++ diag' as bs acc'
                                             where acc' = reverse $ sort (a:acc)

ordered xs = and (zipWith (<) xs (drop 1 xs))

prop_diagSum = forAll gen $ \xs -> all (>0) xs && ordered xs ==> let s = diagSum xs in s == sort s
               where gen = do i <- (arbitrary :: Gen [Int])
                              return $ sort i

deepCheck p = check (defaultConfig { configMaxTest = 10000}) p

--main = print $ filter (not . isSum) [1..28123]
main = print . sum $ diff [1..28123] sums
