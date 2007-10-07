The number of routes through an n * n grid can be visualised like this:

 1, 1, 1, 1, 1...
 1, 2, 3, 4, 5...
 1, 3, 6,10,15...
 1, 4,10,20,35...
 1, 5,15,35,70...
...

where the number of routes is the n + 1'th value along the diagonal.

Funnily enough, that looks like Pascal's triangle rotated 45 degrees... we want
the middle element on row 2n, which is:

C(2n, n) == 2n! / n!n!

> factorial n = factorials !! (n - 1)
>     where factorials = scanl1 (*) [1..]
>
> routes n = factorial (2 * n) `div` factorial (n) ^ 2
> answer = routes 20
> main = print answer
