main = do
  print $ 1:2:[]
  x <- getLine
  if null x
     then return ()
    else do
      putStrLn x
      main


