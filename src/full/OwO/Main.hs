module OwO.Main
  ( runOwO
  ) where

import           OwO.Options
import           OwO.Version    (versionWithCommitInfo)

import           OwO.Util.Monad

runOwO :: CmdOptions -> IO ()
runOwO opts = do
  ifM (optShowVersion opts) printVersion
  return ()

printVersion :: IO ()
printVersion = putStrLn $
  "OwO " ++ versionWithCommitInfo
