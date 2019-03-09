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


--use monoid ie: map each item to monoid then combine all with mappend rules. (easily parraleliable unlek foldr which starts at the end)
--From monoidal parsing lecture

-- data B = B Int Int deriving Eq
data B = B !Int !Int deriving (Eq, Show)

instance Semigroup B where -- same as mappend
  (<>) (B a b) (B c d)
    | b <= c = B (a+c-b) d
    | otherwise = B a (d+b-c)

instance Monoid B where
  mempty = B 0 0
  mappend = (<>)

parse :: Char -> B
parse '(' = B 0 1
parse ')' = B 1 0
parse _   = B 0 0

match' :: String -> Bool
match' xs = foldMap parse xs == B 0 0

--as fold∷
-- match' xs = ((foldr f (B 0 0) xs) == B 0 0) where
--   f :: Char -> B -> B
--   f '(' (B a b)
--     | b<=0      = B (a-b) 1
--     | otherwise = B a (b-1)
--   f ')' (B a b) = B a (b-1)
