diags = 1 : go' 1 2
  where
    go' l i = (l + i) : (l + 2 * i) : (l + 3 * i) : (l + 4 * i) : go' (l + 4 * i) (i + 2)

problem0028 n = sum $ take (1 + (n - 1) * 2) diags

main = print $ problem0028 1001
