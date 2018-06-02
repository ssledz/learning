module Main where

main :: IO ()
main = do
  content <- readFile "numbers.txt"
  putStrLn "---"
  putStrLn content
  -- print is used for diagnostic purposes
  print content
  putStrLn "---"
