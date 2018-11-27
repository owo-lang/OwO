module Main (main) where

import           Control.Applicative
import           Data.Maybe           (fromMaybe)
import           System.Exit          (exitFailure)
import           System.IO            (hPutStrLn, stderr)

import           OwO.Main             (runOwO)
import           OwO.Options          (CompilerOptions (..), PragmaOptions (..))
import           OwO.Util.Applicative
import           OwO.Util.Tools
import           OwO.Version          (versionWithCommitInfo)

import           OptionParser

printVersion :: IO ()
printVersion = putStrLn $ "OwO " ++ versionWithCommitInfo

noInputFile :: CmdOptions -> IO a
noInputFile opts = do
  unlessM (showVersion opts || showHelp opts) $
    hPutStrLn stderr "Please specify an input file."
  exitFailure

main :: IO ()
main = do
  opts <- options
  showVersion opts `ifM` printVersion
  file <- fromMaybe (noInputFile opts) $ pure <$> compilerInputFile opts
  let hideLocation = compilerDumpHideLoc opts
  let toDumpTok = compilerDumpToken opts
  let toDumpAst = compilerDumpAst opts
  ifM toDumpTok $ dumpTokens file hideLocation
  ifM toDumpAst $ dumpAst file hideLocation
  unlessM (toDumpTok || toDumpAst) . runOwO $ CompilerOptions
    { optInputFile     = file
    , optIncludePaths  = compilerIncludePaths opts
    , optPragmaOptions = PragmaOptions
      { optNoPositivityCheck     = pragmaNoPositivityCheck opts
      , optNoTerminationCheck    = pragmaNoTerminationCheck opts
      , optNoExhaustivenessCheck = pragmaNoExhaustivenessCheck opts
      }
    }
