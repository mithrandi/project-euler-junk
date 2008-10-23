import Data.Ratio
import Control.Monad

curious = do
    n <- [1..9]
    c <- [1..9]
    d <- [1..9]
    n' <- [n * 10 + c, c * 10 + n]
    d' <- [d * 10 + c, c * 10 + d]
    guard $ n' `rem` 10 /= 0
    guard $ d' `rem` 10 /= 0
    guard $ (n' % d') == (n % d)
    guard $ (n' % d') < 1
    return (n', d')

problem0033 = denominator . product $ map (uncurry (%)) curious

main = print problem0033
