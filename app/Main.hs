module Main where

import           Cli                      (opts)
import           Control.Lens             ((^.))
import           Katip                    (closeScribes)
import           Lib                      (app)
import           Lib.App                  (runLoggerT)
import           Lib.Effects.Logger       (infoKatip, withContextKatip)
import           Lib.Env                  (AppEnv (AppEnv), loggerLogEnv,
                                           newLoggerEnv, serverEnv, serverPort)
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative      (execParser)
import           Protolude


main :: IO ()
main = (\sEnv -> bracket (makeAppEnv sEnv) stopApp runApp) =<< execParser opts
  where
    makeAppEnv sEnv = do
      loggerEnv <- newLoggerEnv
      pure $ AppEnv sEnv loggerEnv
    runApp env = do
      let port = env^.serverPort
      liftIO . runLoggerT env . withContextKatip (env^.serverEnv) $ infoKatip "Running"
      run port $ app env
    stopApp env = do
      liftIO . runLoggerT env $ infoKatip "Shutting down gracefully"
      closeScribes $ env^.loggerLogEnv
