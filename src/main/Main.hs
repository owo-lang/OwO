module Main (main) where

import           Control.Applicative

import           OwO.Main             (runOwO)
import           OwO.Options
    ( CompilerOptions (..)
    , PragmaOptions (..)
    , defaultPragmaOptions
    )
import           OwO.Util.Applicative
import           OwO.Version          (versionWithCommitInfo)

import           OptionParser

printVersion :: IO ()
printVersion = putStrLn $ "OwO " ++ versionWithCommitInfo

main :: IO ()
main = do
  opts <- options
  showVersion opts `ifM` printVersion
  case compilerInputFile opts of
    Nothing ->
      if showVersion opts || showHelp opts then pure ()
      else putStrLn "Please specify an input file!"
    Just f  -> runOwO $ CompilerOptions
        { optInputFile     = f
        , optIncludePaths  = compilerIncludePaths opts
        , optDumpTokens    = compilerDumpTokens opts
        , optDumpAst       = compilerDumpAst opts
        , optPragmaOptions = defaultPragmaOptions
        }
