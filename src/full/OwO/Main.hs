module OwO.Main
  ( runOwO
  ) where

import           OwO.Options
import           OwO.Util.Applicative
import           OwO.Util.Tools

runOwO :: CompilerOptions -> IO ()
runOwO opts = do
  let file = optInputFile opts
  optDumpTokens opts `ifM` dumpTokens file
  optDumpAst opts `ifM` dumpAst file
  -- TODO
  return ()
