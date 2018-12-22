{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text.IO             as T
import           Lib                      (app)
import           Network.Wai.Handler.Warp (run)


main :: IO ()
main = do
  T.putStrLn "Glossy"
  run 8080 app
