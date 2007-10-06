A Pythagorean triplet is a set of three natural numbers, abc, for which,
a² + b² = c²

For example, 3² + 4² = 9 + 16 = 25 = 5².

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

> isAnswer (a, b) = a ^ 2 + b ^ 2 == (1000 - a - b) ^ 2
>
> solutions = filter isAnswer possibles
>             where possibles = [(a, b) | a <- [1..1000], b <- [a..1000]]
>
> answer (a, b) = a * b * (1000 - a - b)

> main = print . answer . head $ solutions
