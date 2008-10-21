import Euler
import Control.Monad
import Control.Monad.Instances

powerSum e n = sum . map (^e) $ digits n

problem0030 = filter (ap (==) (powerSum 5)) $ [2..]

main = print problem0030
