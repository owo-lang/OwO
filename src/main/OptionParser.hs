module OptionParser
  ( options
  ) where

import Options.Applicative

import OwO.Options

options :: IO CmdOptions
options = execParser $ info (helper <*> opts)
  (  fullDesc
  <> header "Command line tool for the OwO Pwogwamming Langwage."
  )
  where
    opts = CmdOptions
      <$> optional
        (strOption (  long "src"
                   <> help "Source file path"
                   <> short 'c'
                   ))
      <*> many
        (strOption (  long "include-dir"
                   <> help "Include paths"
                   ))
      <*> switch
          (  long "version"
          <> help "Show OwO compiler version"
          <> short 'V'
          )
      <*> switch
          (  long "help"
          <> help "Print this message"
          <> short 'h'
          )
