module OptionParser
  ( options
  ) where

import           Options.Applicative

import           OwO.Options

options :: IO CmdOptions
options = customExecParser pref $ info (helper <*> opts)
  (  fullDesc
  <> header "Command line tool for the OwO Pwogwamming Langwage."
  )
  where
    pref = prefs showHelpOnError
    opts = CmdOptions
      <$> optional
        (strOption (  long "src"
                   <> help "Source file path"
                   <> metavar "PATH"
                   <> short 'c'
                   ))
      <*> many
        (strOption (  long "include-dir"
                   <> help "Include paths"
                   <> metavar "DIR"
                   <> short 'I'
                   ))
      <*> switch
          (  long "version"
          <> help "Show OwO compiler wersion"
          <> short 'V'
          )
      <*> switch
          (  long "help"
          <> help "Pwint this message"
          <> short 'h'
          )
