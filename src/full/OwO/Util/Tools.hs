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

simpleToken :: PsiToken -> String
simpleToken token = show (tokenType token)

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file isSimple = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens -> mapM_ putStrLn $ case isSimple of
      True  -> simpleToken <$> tokens
      False -> prettyToken <$> tokens

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file isSimple = __TODO__
