import Control.Applicative
import Data.List
import Data.Char

choices :: [a] -> [(a, [a])]
choices = map (\(x,y) -> (head y, x ++ tail y)) . init . (zip <$> inits <*> tails)

permutations :: [a] -> [[a]]
permutations []   = [[]]
permutations xs   = do (x, rest) <- choices xs
                       map (x:) (permutations rest)

main = print $ map intToDigit (permutations [0..9] !! 999999)
