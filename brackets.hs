expr1 = "3([241(1]12))" -- ([(]))
expr2= "(fd[fg(21)]1)" -- ([()])


match :: String -> String -> Bool
match [] [] = True
match [] _ = False
match (')':xs) (y:ys) = and [(y==')'), match xs ys]
match (']':xs) (y:ys) = and [(y==']'), match xs ys]
match ('}':xs) (y:ys) = and [(y=='}'), match xs ys]
match (x:xs) ys
  |( x=='(') = match xs (')':ys)
  | (x=='[') = match xs (']':ys)
  | (x=='{') = match xs ('}':ys)
  | otherwise = match xs ys
