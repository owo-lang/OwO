{-# LANGUAGE LambdaCase #-}
module OwO.Util.Tools
  ( dumpTokens
  , dumpAst
  ) where

import           OwO.Syntax.Parser
import           OwO.Syntax.Position
import           OwO.Syntax.TokenType

import           Prelude              hiding (lex)
import           System.Exit          (exitFailure)
import           System.IO

prettyToken :: PsiToken -> String
-- TODO add line, col information
prettyToken token = show (tokenType token)

dumpTokens :: FilePath -> IO ()
dumpTokens file = lex <$> readFile file >>= \case
  Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
  Right tokens -> mapM_ putStrLn $ prettyToken <$> tokens

dumpAst :: FilePath -> IO ()
dumpAst = error "unimplemented"
