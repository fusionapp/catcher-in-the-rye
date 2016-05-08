module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

import App
import Lib

main :: IO ()
main = do
  configPath <- lookupEnv "CATCHER_CONFIG"
  config <- either (fail . prettyPrintParseException) return =<<
    decodeFileEither (fromMaybe "config.yaml" configPath)
  app <- runStderrLoggingT (mkApp config)
  run 8080 (serveApp app)
