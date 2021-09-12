{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module NodeCli where

import System.Process
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader


import Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BLU

data NodeCliConfig = NodeCliConfig {nlcOutDir :: String, nlcNetwork :: String, nlcContainerName :: String, nlcDockerImage :: String}

cliDefaultConfig :: NodeCliConfig
cliDefaultConfig =
  NodeCliConfig
    {   nlcOutDir = "/home/ssledz/now/out"
      , nlcNetwork = "testnet"
      , nlcContainerName = "node-cli"
      , nlcDockerImage = "inputoutput/cardano-node:1.29.0-rc2"
    }

tip :: ReaderT NodeCliConfig IO String
tip = do cfg <- ask
         liftIO $ cli cfg [ "query", "tip", "--testnet-magic", "8"]

cli :: NodeCliConfig -> [String] -> IO String
cli cfg args = readProcess "docker" (args' <> args) ""
  where
    args' =
      [ "run",
        "--name",
        nlcContainerName cfg,
        "--rm",
        "--entrypoint",
        "cardano-cli",
        "-e",
        "NETWORK=" <> nlcNetwork cfg,
        "-e",
        "CARDANO_NODE_SOCKET_PATH=/ipc/socket",
        "-v",
        "node-ipc:/ipc",
        "-v",
        nlcOutDir cfg,
        nlcDockerImage cfg
      ]

type CliAPI = "tip" :> Get '[JSON] (Maybe Tip)

data Tip = Tip 
  { epoch :: Integer
  , hash :: String
  , slot :: Integer
  , block :: Integer
  , era :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Tip
instance FromJSON Tip

cliServer :: Server CliAPI
cliServer = do
    json <- liftIO $ runTip
    let parsed = decode (BLU.fromString json) :: Maybe Tip
    liftIO $ putStrLn json
    liftIO $ putStrLn (show parsed)
    -- return $ Just (Tip 3111 "aasx" 1 2 "alonzo")
    return parsed
    where
        runTip = runReaderT tip cliDefaultConfig

cliAPI :: Proxy CliAPI
cliAPI = Proxy

cliApp :: Application
cliApp = serve cliAPI cliServer