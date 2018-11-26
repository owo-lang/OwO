{-# LANGUAGE CPP        #-}
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
#include <impossible.h>

prettyToken :: PsiToken -> String
prettyToken token = show (tokenType token) ++ " " ++
    printLoc (iStart loc) ++ " " ++ printLoc (iEnd loc)
  where
    loc = location token
    printLoc loc = "(" ++ show (posPos  loc) ++
                   " " ++ show (posLine loc) ++
                   " " ++ show (posCol  loc) ++ ")"

dumpTokens :: FilePath -> IO ()
dumpTokens file = lex <$> readFile file >>= \case
  Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
  Right tokens -> mapM_ putStrLn $ prettyToken <$> tokens

dumpAst :: FilePath -> IO ()
dumpAst = __TODO__
