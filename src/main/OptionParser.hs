module OptionParser
  ( options
  , CmdOptions(..)
  ) where

import           Options.Applicative

import           OwO.Options

data CmdOptions = CmdOptions
  { compilerInputFile    :: Maybe FilePath
  , compilerIncludePaths :: [FilePath]
  , compilerDumpTokens   :: Bool
  , compilerDumpAst      :: Bool
  , showVersion          :: Bool
  , showHelp             :: Bool
  , pragmaSafe           :: Bool
  }

options :: IO CmdOptions
options = customExecParser pref information
  where
    information = info (helper <*> opts)
      $  fullDesc
      <> header "Command line tool for the OwO Pwogwamming Langwage."
    pref = prefs $  showHelpOnError
                 <> showHelpOnEmpty
                 <> disambiguate
    opts = CmdOptions
      <$> optional
        (strOption $  long "src"
                   <> help "Source file path"
                   <> metavar "PATH"
                   <> short 'c'
                   )
      <*> many
        (strOption $  long "include-dir"
                   <> help "Include paths"
                   <> metavar "DIR"
                   <> short 'I'
                   )
      <*> switch
          (  long "dump-tokens"
          <> help "Lex the file and print the tokens"
          )
      <*> switch
          (  long "dump-ast"
          <> help "Parse the file and print the abstract syntax tree"
          )
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
      <*> switch
          (  long "safe"
          <> help "Enable safe mode"
          )
