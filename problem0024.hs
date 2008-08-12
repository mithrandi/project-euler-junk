import Control.Applicative
import Data.List
import Data.Char

choices :: [a] -> [(a, [a])]
choices = map (\(x,y) -> (head y, x ++ tail y)) . init . (zip <$> inits <*> tails)

choose :: Int -> [a] -> (a, [a])
choose n xs = let (l, x:r) = splitAt n xs
              in (x, l ++ r)

permutations :: [a] -> [[a]]
permutations []   = [[]]
permutations xs   = do (x, rest) <- choices xs
                       map (x:) (permutations rest)

fact n = product [1..n]

toFactoradic 1 _ = [0]
toFactoradic d n = a : toFactoradic (d - 1) b
                   where (a, b) = divMod n (fact (d - 1))

permute xs n = go xs (toFactoradic (length xs) n)
    where go xs (f:fs) = let (c,cs) = choose f xs
                         in (c : go cs fs)
          go _  []     = []

--main = print $ map intToDigit (permutations [0..9] !! 999999)
main = print . map intToDigit $ permute [0..9] 999999
