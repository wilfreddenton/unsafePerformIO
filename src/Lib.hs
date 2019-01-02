module Lib where

import           Control.Lens             ((^.))
import           Katip                    (closeScribes)
import           Lib.App                  (runLoggerT)
import           Lib.Effects.Logger       (infoKatip, withContextKatip)
import           Lib.Env                  (AppEnv (AppEnv), ServerEnv, lLogEnv,
                                           newDbEnv, newLoggerEnv, sePort,
                                           serverEnv)
import           Lib.Server               (app)
import           Network.Wai.Handler.Warp (run)
import           Protolude

initialize :: ServerEnv -> IO ()
initialize serverEnv' = bracket makeAppEnv stopApp runApp
  where
    makeAppEnv = do
      loggerEnv <- newLoggerEnv
      dbEnv <- newDbEnv
      pure $ AppEnv serverEnv' loggerEnv dbEnv
    runApp env = do
      let port = env^.sePort
      liftIO . runLoggerT env . withContextKatip (env^.serverEnv) $ infoKatip "Running"
      run port $ app env
    stopApp env = do
      liftIO . runLoggerT env $ infoKatip "Shutting down gracefully"
      closeScribes $ env^.lLogEnv
