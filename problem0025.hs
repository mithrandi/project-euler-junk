import Data.List
import Data.Maybe

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

answer = fromJust . find (\(n, x) -> length (show x) >= 1000) $ zip [0..] fibs

main = print answer
