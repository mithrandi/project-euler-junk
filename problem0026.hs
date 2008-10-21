import Data.List
import Data.Function

rems m n = unfoldr go' m
  where
    go' x = case r of
                0 -> Nothing
                _ -> Just (r, r * 10)
        where r = rem x n

findCount x xs =
  if x `elem` xs
    then Just $ length (takeWhile (/= x) xs) + 1
    else Nothing

countRecurrence m n = go' [] (rems m n)
  where
    go' lxs (rx:rxs) =
        case findCount rx lxs of
            Nothing -> go' (rx:lxs) rxs
            Just n -> n
    go' _ [] = 0

problem26 = maximumBy (compare `on` (countRecurrence 1))

main = print $ problem26 [2..999]
