expr1 = "3([241(1]12))"
expr2= "(fd[fg(21)]1)"


match :: String -> Int -> Bool
match [] 0 = True
match [] _ = False
match (x:xs) n
  | n < 0 = False
  | x=='(' = match xs (n+1)
  | x==')' = match xs (n-1)
  | otherwise = match xs n
