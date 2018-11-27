module Main (main) where

import           Control.Applicative
import           System.IO            (hPutStrLn, stderr)

import           OwO.Main             (runOwO)
import           OwO.Options          (CompilerOptions (..), PragmaOptions (..))
import           OwO.Util.Applicative
import           OwO.Util.Tools
import           OwO.Version          (versionWithCommitInfo)

import           OptionParser

printVersion :: IO ()
printVersion = putStrLn $ "OwO " ++ versionWithCommitInfo

main :: IO ()
main = do
  opts <- options
  showVersion opts `ifM` printVersion
  case compilerInputFile opts of
    Nothing   ->
      if showVersion opts || showHelp opts then pure ()
      else hPutStrLn stderr "Please specify an input file!"
    Just file -> do
      let nDumpTokLevel = compilerDumpTokVerbose opts
      let toDumpAst = compilerDumpAst opts
      dumpTokens nDumpTokLevel file
      ifM toDumpAst $ dumpAst file
      unlessM ((nDumpTokLevel > 0) || toDumpAst) . runOwO $ CompilerOptions
        { optInputFile     = file
        , optIncludePaths  = compilerIncludePaths opts
        , optPragmaOptions = PragmaOptions
          { optNoPositivityCheck     = pragmaNoPositivityCheck opts
          , optNoTerminationCheck    = pragmaNoTerminationCheck opts
          , optNoExhaustivenessCheck = pragmaNoExhaustivenessCheck opts
          }
        }
