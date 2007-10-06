> import Data.List
> import Control.Monad
> import Control.Monad.Instances

The sum of the squares of the first ten natural numbers is,
1² + 2² + ... + 10² = 385

> sumOfSquares ns = foldr1 (+) $ map (^2) ns

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)² = 55² = 3025

> squareOfSum ns = (foldr1 (+) ns) ^ 2

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 - 385 = 2640.

> diff = liftM2 (-) squareOfSum sumOfSquares

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.

> main = do print $ diff [1..10]
>           print $ diff [1..100]
