import Control.Applicative
import Data.List
import Data.Char
import Euler

toFactoradic 1 _ = [0]
toFactoradic d n = a : toFactoradic (d - 1) b
                   where (a, b) = divMod n (fact (d - 1))

permute xs n = go xs (toFactoradic (length xs) n)
    where go xs (f:fs) = let (c,cs) = choose f xs
                         in (c : go cs fs)
          go _  []     = []

--main = print $ map intToDigit (permutations [0..9] !! 999999)
main = print . map intToDigit $ permute [0..9] 999999
