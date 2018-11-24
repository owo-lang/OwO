module Main where

-- import Test.Hspec

import           Text.Pretty.Simple (pPrint)

import           OwO.Syntax.Parser
import           Prelude            hiding (lex)

lexAndPrint = pPrint . lex

main :: IO ()
main = do
  lexAndPrint $ "module Bla where\n" ++
                "  a = b"
  lexAndPrint $ "data Bla : Type where\n" ++
                "  Rua : B"
  lexAndPrint $ "codata Bla : Type where\n" ++
                "  oRA : B"
