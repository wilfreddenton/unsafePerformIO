module Cli where

import           Lib.Env             (ServerEnv (ServerEnv))
import           Options.Applicative (Parser, ParserInfo, auto, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      option, progDesc, short, strOption,
                                      (<**>))
import           Protolude           hiding (option)

opts :: ParserInfo ServerEnv
opts = info (command <**> helper) (
  fullDesc <>
  progDesc "Boilerplate for a Haskell web service" <>
  header "Haskell Start Pack"
  )

command :: Parser ServerEnv
command = ServerEnv <$> option auto (
  long "port" <>
  short 'p' <>
  metavar "PORT" <>
  help "The port the server runs on"
  ) <*> strOption (
  long "sqlite" <>
  short 's' <>
  metavar "SQLITE DATABASE" <>
  help "File path of the sqlite database"
  ) <*> strOption (
  long "gnupg-homedir" <>
  short 'h' <>
  metavar "HOMEDIR" <>
  help "GnuPG homedir"
  )
