import Data.List


numberInWords :: Integer -> [String]
numberInWords n | n == 0           = []
                | n < 20           = [number n]
                | n == 1000        = ["one", "thousand"]
                | n `mod` 100 == 0 = [number (n `div` 100), "hundred"]
                | n > 100          = let h = n `div` 100
                                     in number h : "hundred" : "and" : numberInWords (n - h * 100)
                | n >= 20          = let t = n `div` 10
                                     in tens t : numberInWords (n - t * 10)
    where number n = ["one", "two", "three", "four", "five", "six", "seven",
                      "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                      "fourteen", "fifteen", "sixteen", "seventeen",
                      "eighteen", "nineteen"] !! fromIntegral (n - 1)
          tens n   = ["twenty", "thirty", "fourty", "fifty", "sixty",
                      "seventy", "eighty", "ninety"] !! fromIntegral (n - 2)

lengthInWords :: Integer -> Int
lengthInWords = length . concat . numberInWords

answer = sum $ map lengthInWords [1..1000]
main = print answer
