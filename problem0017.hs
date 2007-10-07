import Data.List

numberInWords :: Integer -> [String]
numberInWords = concat . unfoldr word
    where word n | n == 0           = Nothing
                 | n == 1           = Just (["one"], 0)
                 | n == 2           = Just (["two"], 0)
                 | n == 3           = Just (["three"], 0)
                 | n == 4           = Just (["four"], 0)
                 | n == 5           = Just (["five"], 0)
                 | n == 6           = Just (["six"], 0)
                 | n == 7           = Just (["seven"], 0)
                 | n == 8           = Just (["eight"], 0)
                 | n == 9           = Just (["nine"], 0)
                 | n == 10          = Just (["ten"], 0)
                 | n == 11          = Just (["eleven"], 0)
                 | n == 12          = Just (["twelve"], 0)
                 | n == 13          = Just (["thirteen"], 0)
                 | n == 14          = Just (["fourteen"], 0)
                 | n == 15          = Just (["fifteen"], 0)
                 | n == 16          = Just (["sixteen"], 0)
                 | n == 17          = Just (["seventeen"], 0)
                 | n == 18          = Just (["eighteen"], 0)
                 | n == 19          = Just (["nineteen"], 0)
                 | n == 1000        = Just (["one", "thousand"], 0)
                 | n `mod` 100 == 0 = do ([h], _) <- word (n `div` 100)
                                         return ([h, "hundred"], 0)
                 | n > 100          = let h = n `div` 100 in
                                      do ([wh], _) <- word h
                                         return ([wh, "hundred", "and"], n - (h * 100))
                 | n >= 90          = Just (["ninety"], n - 90)
                 | n >= 80          = Just (["eighty"], n - 80)
                 | n >= 70          = Just (["seventy"], n - 70)
                 | n >= 60          = Just (["sixty"], n - 60)
                 | n >= 50          = Just (["fifty"], n - 50)
                 | n >= 40          = Just (["forty"], n - 40)
                 | n >= 30          = Just (["thirty"], n - 30)
                 | n >= 20          = Just (["twenty"], n - 20)

lengthInWords :: Integer -> Int
lengthInWords = length . concat . numberInWords

answer = sum $ map lengthInWords [1..1000]
main = print answer
