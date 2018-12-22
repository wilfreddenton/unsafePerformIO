{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cli                      (opts)
import           Control.Exception        (bracket)
import           Control.Lens             ((^.))
import           Data.Text.IO             as T
import           Lib                      (app)
import           Lib.Env                  (AppEnv (AppEnv), newLoggerEnv,
                                           serverPort)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)


main :: IO ()
main = (\serverEnv -> bracket (makeAppEnv serverEnv) stopApp runApp) =<< execParser opts
  where
    makeAppEnv serverEnv = do
      T.putStrLn "starting up"
      loggerEnv <- newLoggerEnv
      pure $ AppEnv serverEnv loggerEnv
    stopApp _ = T.putStrLn $ "shutting down"
    runApp env = run (env^.serverPort) $ app env
