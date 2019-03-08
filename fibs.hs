fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib :: Int -> Int
fib n = fibs!!n
