module Main where

-- import           Test.Hspec
-- import           Text.Pretty.Simple (pPrint)

-- import           OwO.Syntax.Parser
-- import           Prelude            hiding (lex)

import           System.Exit    (ExitCode (..))
import           System.Process (runProcess, waitForProcess)

-- lexAndPrint = pPrint . lex

main :: IO ()
main = do
  putStrLn ""
  handle <- runProcess "perl" ["test_runner.pl"]
    (Just "src/test/testData") Nothing Nothing Nothing Nothing
  code <- waitForProcess handle
  return ()
