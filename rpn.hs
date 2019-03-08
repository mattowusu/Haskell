-- reverse polish notation evaluator

rpn :: String -> Int
rpn sttr = head $ foldl f [] (words sttr) where
  f :: [Int] -> String -> [Int]
  f (b1:b2:bs) "*" = (b1*b2):bs
  f (b1:b2:bs) "+" = (b1+b2):bs
  f bs n = (read n):bs
