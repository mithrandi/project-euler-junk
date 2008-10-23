import Data.Ratio
import Control.Monad

curious = do
    n <- [1..9]
    c <- [1..9]
    d <- [1..9]
    n' <- [n * 10 + c, c * 10 + n]
    d' <- [d * 10 + c, c * 10 + d]
    let f = (n' % d')
    guard $ f == (n % d) && f < 1
    return f

problem0033 = denominator . product $ curious

main = print problem0033
