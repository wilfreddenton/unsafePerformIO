module Lib where

import           Control.Lens                    ((^.))
import           Database.SQLite.Simple          (close)
import           Katip                           (closeScribes)
import           Lib.App                         (runLoggerT)
import           Lib.Effects.Logger              (infoKatip, withContextKatip)
import           Lib.Env                         (AppEnv (AppEnv), ServerEnv,
                                                  dConn, lLogEnv, newAuthEnv,
                                                  newDbEnv, newLoggerEnv,
                                                  sGnuPgHomedir, sInitSql,
                                                  sPgpPublicKey, sPort,
                                                  sSqliteDatabase, serverEnv)
import           Lib.Server                      (app)
import           Network.Wai.Handler.Warp        (run)
import           Network.Wai.Middleware.Throttle (defaultThrottleSettings,
                                                  initThrottler, throttle,
                                                  throttleSettingsBurst,
                                                  throttleSettingsRate)
import           Protolude
import           System.Clock                    (TimeSpec (TimeSpec))

initialize :: ServerEnv -> IO ()
initialize serverEnv' = bracket makeAppEnv stopApp runApp
  where
    makeAppEnv = do
      dbEnv <- newDbEnv False (serverEnv'^.sSqliteDatabase) $ serverEnv'^.sInitSql
      authEnv <- newAuthEnv (serverEnv'^.sGnuPgHomedir) $ serverEnv'^.sPgpPublicKey
      loggerEnv <- newLoggerEnv
      pure $ AppEnv serverEnv' loggerEnv dbEnv authEnv
    runApp env = do
      let port = env^.sPort
      throttler <- initThrottler (defaultThrottleSettings $ TimeSpec 5 0) { throttleSettingsRate = 15, throttleSettingsBurst = 20 }
      let f = throttle throttler
      liftIO . runLoggerT env . withContextKatip (env^.serverEnv) $ infoKatip "Running"
      run port . f $ app env
    stopApp env = do
      liftIO . runLoggerT env $ infoKatip "Shutting down gracefully"
      liftIO . close $ env^.dConn
      closeScribes $ env^.lLogEnv
