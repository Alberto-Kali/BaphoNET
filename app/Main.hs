{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import TransformController (extractContent, ExtractionRequest(..), ExtractionResponse(..))

import Servant
import Servant.Server
import Network.Wai.Handler.Warp
import Data.Aeson
import Control.Monad.IO.Class

type SquareAPI = "hello" :> Get '[JSON] String
            :<|> "transform" :> ReqBody '[JSON] ExtractionRequest :> Post '[JSON] ExtractionResponse

squareAPI :: Proxy SquareAPI
squareAPI = Proxy

server :: Server SquareAPI
server = helloHandler :<|> transformHandler

helloHandler :: Handler String
helloHandler = return "Hi"

transformHandler :: ExtractionRequest -> Handler ExtractionResponse
transformHandler body = liftIO $ extractContent body

main :: IO ()
main = do
    putStrLn "Starting server on port 8080..."
    run 8080 (serve squareAPI server)