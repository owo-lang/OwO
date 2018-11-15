{-# LANGUAGE CPP #-}
module OwO.TypeChecking.Monad where

import Control.Monad
import Control.Monad.Trans.Except

#include <impossible.h>

data TCState = TypeCheckingState
  {
  } deriving Show

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT Err m)

-- | TypeChecking Monad
type TCM = TCMT IO

