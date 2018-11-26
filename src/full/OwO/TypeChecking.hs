{-# LANGUAGE CPP #-}

module OwO.TypeChecking where

import           OwO.Syntax.Abstract
import           OwO.TypeChecking.Core
import           OwO.TypeChecking.Match
import           OwO.TypeChecking.Monad
import           OwO.TypeChecking.Reduce

#include <impossible.h>

typeCheck :: TCEnv -> PsiTerm -> Either TCErr Term
typeCheck env term = __TODO__

typeCheckFile :: TCState -> PsiFile -> TCM ()
typeCheckFile state file = do
  __TODO__
