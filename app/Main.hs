module Main where

import           Cli                      (opts)
import           Control.Lens             ((^.))
import           Katip                    (closeScribes)
import           Lib                      (app)
import           Lib.App                  (runLoggerT)
import           Lib.Effects.Logger       (logKatip)
import           Lib.Env                  (AppEnv (AppEnv), loggerLogEnv,
                                           newLoggerEnv, serverPort)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Protolude


main :: IO ()
main = (\serverEnv -> bracket (makeAppEnv serverEnv) stopApp runApp) =<< execParser opts
  where
    info env = liftIO . runLoggerT env . logKatip
    makeAppEnv serverEnv = do
      loggerEnv <- newLoggerEnv
      pure $ AppEnv serverEnv loggerEnv
    runApp env = do
      let port = env^.serverPort
      info env $ "Running on port " <> show port
      run port $ app env
    stopApp env = do
      info env "Shutting down gracefully"
      closeScribes $ env^.loggerLogEnv
