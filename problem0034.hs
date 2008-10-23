import Euler
import Control.Monad

factDigits n = sum . (map fact) $ digits n

problem0034 = scanl (+) 0 $ filter (ap (==) factDigits) [10..]

main = print problem0034
