{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp ( run, Port )



type API = "hopp" :> Get '[JSON] Int

portNr :: Port
portNr = 8081

startApp :: IO ()
startApp = do
  putStrLn $  "Server running on port : " <> show portNr
  
  run portNr app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return 5

main :: IO ()
main = do
  startApp