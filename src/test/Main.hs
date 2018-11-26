module Main where

-- import           Test.Hspec
-- import           Text.Pretty.Simple (pPrint)

-- import           OwO.Syntax.Parser
-- import           Prelude            hiding (lex)

import           System.Exit    (ExitCode (..))
import           System.Process (system)

-- lexAndPrint = pPrint . lex

main :: IO ()
main = do
  ExitSuccess <- system "bash -c 'cd ./src/test/testData; ./test_runner.pl'"
  return ()
