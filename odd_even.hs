main :: IO ()
main = return ()

-- ##########################
-- use mutual recurison
odd_p :: Int -> Bool
odd_p 0 = False
odd_p n = even_p (n-1)

even_p :: Int -> Bool
even_p 0 = True
even_p n = odd_p (n-1)

-- use fold along characters(Finite automata approach)
divides :: Int -> Int -> Bool
divides d n = foldl (fsm_div d) True (digs n)

fsm_div :: Int -> Bool -> Int -> Bool
fsm_div b n

digs :: Int -> [Int]
digs 0 = []
digs x = (digs (x `div` 10)) ++ [x `mod` 10]
