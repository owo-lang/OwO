module OwO.Syntax.Parser where

import           Prelude                 hiding (lex)
import           System.FilePath

import           OwO.Syntax.Parser.Lexer
import           OwO.Syntax.TokenType

scanAll :: Alex [PsiToken]
scanAll = do
  t <- alexMonadScan
  case tokenType t of
    EndOfFileToken -> pure []
    _              -> (t :) <$> scanAll

-- | Returning error message or a list of tokens
lex :: String -> Either String [PsiToken]
lex = flip runAlex scanAll