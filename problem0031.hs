coins = [200, 100, 50, 20, 10, 5, 2, 1]

compose n = compose' n coins
  where
    compose' 0 _      = [[]]
    compose' _ []     = fail "This shouldn't happen!"
    compose' n (c:cs) =
        if c > n
          then compose' n cs
          else (map (c:) (compose' (n - c) (c:cs))) ++ compose' n cs

problem0031 = length . compose $ 200

main = print problem0031
