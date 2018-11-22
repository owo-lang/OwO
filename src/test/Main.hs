module Main where

-- import Test.Hspec

import Prelude hiding (lex)
import OwO.Syntax.Parser

main :: IO ()
main = print . lex $
 "module A where\n" ++
 "a = b"
