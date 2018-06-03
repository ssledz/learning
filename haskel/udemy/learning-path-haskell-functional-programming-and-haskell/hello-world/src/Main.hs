module Main where

main :: IO ()
main = do
  content <- readFile "numbers.txt"
  putStrLn "---"
  putStrLn content
  -- print is used for diagnostic purposes
  print content
  putStrLn "---"

myfun = do
  let x = 1
  putStrLn $ show x

