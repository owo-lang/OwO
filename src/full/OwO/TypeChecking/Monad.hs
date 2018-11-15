{-# LANGUAGE CPP #-}
module OwO.TypeChecking.Monad where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

#include <impossible.h>

data TCState = TypeCheckingState
  {
  } deriving Show

data TCErr = OtherErr String
  deriving (Eq, Show)

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO

