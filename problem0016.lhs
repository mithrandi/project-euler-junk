> import Data.Char

    What is the sum of the digits of the number 2 ^ 1000?

Simple one-liner:

> answer = foldl1 (+) $ map digitToInt $ show $ 2 ^ 1000
> main = print answer
