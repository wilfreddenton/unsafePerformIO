module Main where

import           Cli                      (opts)
import           Control.Lens             ((^.))
import           Katip                    (closeScribes)
import           Lib                      (app)
import           Lib.Env                  (AppEnv (AppEnv), loggerLogEnv,
                                           newLoggerEnv, serverPort)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Protolude


main :: IO ()
main = (\serverEnv -> bracket (makeAppEnv serverEnv) stopApp runApp) =<< execParser opts
  where
    makeAppEnv serverEnv = do
      putText "starting up"
      loggerEnv <- newLoggerEnv
      pure $ AppEnv serverEnv loggerEnv
    stopApp env = do
      _ <- closeScribes (env^.loggerLogEnv)
      putText $ "shutting down"
    runApp env = run (env^.serverPort) $ app env
