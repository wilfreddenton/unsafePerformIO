module Main where

import           Cli                 (opts)
import           Crypto.Gpgme
import           Lib                 (initialize)
import           Options.Applicative (execParser)
import           Prelude             (String)
import           Protolude


main :: IO ()
main = do
  withCtx "/home/wilfred/Documents/haskell-starter-pack/.gnupg" "C" OpenPGP $ \ctx -> do
    ks <- listKeys ctx NoSecret
    putStrLn $ (show $ length ks :: String)
  initialize =<< execParser opts
