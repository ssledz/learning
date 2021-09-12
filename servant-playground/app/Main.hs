module Main where

import qualified MyLib (someFunc)
import Server (app1, app2, app3, app4, app5, app6, app7, runApp)
import NodeCli
import System.Process
import Control.Monad.Trans.Reader

main :: IO ()
-- main = do
--   putStrLn "Starting server on port 8081..."
--   runApp app7 8081

main = do
  putStrLn "Starting server on port 8081..."
  -- currenDir <- readProcess "pwd" [] ""
  -- putStrLn $ show currenDir
  -- res <- cli cliDefaultConfig [ "query","tip", "--testnet-magic", "8"]
  -- res2 <- runReaderT tip cliDefaultConfig
  -- putStrLn res
  -- putStrLn res2
  runApp cliApp 8081