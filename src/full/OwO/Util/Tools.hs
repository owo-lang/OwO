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
prettyToken token = simpleToken token ++ " " ++
    printLoc (iStart loc) ++ " " ++ printLoc (iEnd loc)
  where
    loc = location token
    printLoc loc = "(" ++ show (posPos  loc) ++
                   " " ++ show (posLine loc) ++
                   " " ++ show (posCol  loc) ++
                   ")"

simpleToken :: PsiToken -> String
simpleToken = show . tokenType

dumpTokens :: FilePath -> Bool -> IO ()
dumpTokens file hideLocation = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens ->
      let f = if hideLocation then simpleToken else prettyToken
      in mapM_ putStrLn $ f <$> tokens

dumpAst :: FilePath -> Bool -> IO ()
dumpAst file hideLocation = __TODO__
