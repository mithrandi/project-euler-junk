A palindromic number reads the same both ways.

> palindromic n = s == reverse s
>                 where s = show n
>
> products n = [x * y | x <- [1..n], y <- [1..n]]

The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.

> largest = maximum . filter palindromic . products

> largest2digits = largest 99

Find the largest palindrome made from the product of two 3-digit numbers.

> answer = largest 999

> main = do print largest2digits
>           print answer
