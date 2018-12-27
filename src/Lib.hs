module Lib where

import           Control.Lens             ((^.))
import           Katip                    (closeScribes)
import           Lib.App                  (runLoggerT)
import           Lib.Effects.Logger       (infoKatip, withContextKatip)
import           Lib.Env                  (AppEnv (AppEnv), ServerEnv,
                                           loggerLogEnv, newLoggerEnv,
                                           serverEnv, serverPort)
import           Lib.Server               (app)
import           Network.Wai.Handler.Warp (run)
import           Protolude

initialize :: ServerEnv -> IO ()
initialize serverEnv' = bracket makeAppEnv stopApp runApp
  where
    makeAppEnv = do
      loggerEnv <- newLoggerEnv
      pure $ AppEnv serverEnv' loggerEnv
    runApp env = do
      let port = env^.serverPort
      liftIO . runLoggerT env . withContextKatip (env^.serverEnv) $ infoKatip "Running"
      run port $ app env
    stopApp env = do
      liftIO . runLoggerT env $ infoKatip "Shutting down gracefully"
      closeScribes $ env^.loggerLogEnv
