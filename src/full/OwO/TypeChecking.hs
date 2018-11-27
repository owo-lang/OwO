{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module OwO.TypeChecking where

import           Data.Functor            ((<&>))
import           Data.Maybe              (catMaybes)

import           OwO.Syntax.Abstract
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

typeCheck :: TCEnv -> PsiTerm -> Either TCErr Term
typeCheck env term = return __TODO__

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file@(PsiFile _ moduleName decls) = do
  let typeSignatures = catMaybes $ decls <&> \case
        (PsiType loc name pragmas t) -> Just (loc, name, pragmas, t)
        _ -> Nothing
  -- TODO:
  --  Look for definitions, give warnings about unimplemented definitions
  --  Then type check the implemented ones
  return __TODO__
