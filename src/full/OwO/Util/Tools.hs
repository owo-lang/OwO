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

dumpTokens :: Int -> FilePath -> IO ()
dumpTokens n file
  | n > 0 && n < 3 = lex <$> readFile file >>= \case
    Left  errMsg -> hPutStrLn stderr errMsg >> exitFailure
    Right tokens -> mapM_ putStrLn $ case n of
      1 -> simpleToken <$> tokens
      2 -> prettyToken <$> tokens
  | otherwise = do
      hPutStrLn stderr ("error: invalid --dump-tokens verbosity " ++ show n)
      exitFailure

dumpAst :: FilePath -> IO ()
dumpAst = __TODO__
