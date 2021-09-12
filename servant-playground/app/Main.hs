module Main where

import qualified MyLib (someFunc)
import Server (runApp, app1, app2, app3, app4, app5, app6, app7)

main :: IO ()
main = do
  putStrLn "Starting server on port 8081..."
  runApp app7 8081
