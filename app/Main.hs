module Main where

import Cli (opts)
import Lib (initialize)
import Options.Applicative (execParser)
import Protolude

main :: IO ()
main = initialize =<< execParser opts
