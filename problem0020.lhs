> import Data.Char

> factorial n = factorials !! (n - 1)
>     where factorials = scanl1 (*) [1..]
>
> answer = foldl1 (+) $ map digitToInt $ show $ factorial 100
> main = print answer
