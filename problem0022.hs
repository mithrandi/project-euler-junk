import Data.Char
import Data.List
import IO

alphaValue :: String -> Int
alphaValue = sum . map (\c -> ord c - ord 'A' + 1)

scores :: [String] -> [Int]
scores = zipWith (*) [1..] . map alphaValue

main = do h <- openFile "names.txt" ReadMode
          s <- hGetContents h
          let names = sort $ read ("[" ++ s ++ "]")
          print $ sum $ scores names
