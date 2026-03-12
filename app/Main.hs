module Main where

import BaphoNET.Api (app)
import BaphoNET.Config (loadConfig)
import BaphoNET.Domain (AppEnv(..), AppState(..))

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    cfg <- loadConfig
    stateVar <- newTVarIO AppState {jobs = Map.empty, nextJobNumber = 1}
    let env = AppEnv {config = cfg, state = stateVar}
    putStrLn "Starting BaphoNET server on port 8080..."
    run 8080 (app env)
