-- ghc --make io
-- ./io or io.exe in windows

-- main = putStrLn "hello, world"
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
