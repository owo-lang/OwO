module Main where

-- import           Test.Hspec
-- import           Text.Pretty.Simple (pPrint)

import           System.Exit    (ExitCode (..), exitWith)
import           System.Process
    ( CreateProcess (..)
    , createProcess
    , proc
    , waitForProcess
    )

checkExit :: ExitCode -> IO ()
checkExit ExitSuccess = pure ()
checkExit n           = exitWith n

main :: IO ()
main = do
  putStrLn ""
  (_, _, _, h) <- createProcess (proc "perl" ["test_runner.pl"])
    { cwd = Just "src/test"
    }
  checkExit <$> waitForProcess h
  pure ()
