module Main where

-- import           Test.Hspec
-- import           Text.Pretty.Simple (pPrint)

-- import           OwO.Syntax.Parser
-- import           Prelude            hiding (lex)

import           System.Exit    (ExitCode (..))
import           System.Process (CreateProcess (..), createProcess, proc,
                                 waitForProcess)

-- lexAndPrint = pPrint . lex

main :: IO ()
main = do
  putStrLn ""
  (_, _, _, h) <- createProcess (proc "perl" ["test_runner.pl"])
    { cwd = Just "src/test/testData"
    }
  ExitSuccess <- waitForProcess h
  return ()
