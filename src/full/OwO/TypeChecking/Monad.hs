{-# LANGUAGE CPP #-}
module OwO.TypeChecking.Monad where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

import OwO.Options

#include <impossible.h>

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { commandLineOptions :: CmdOptions
  } deriving Show

data TCErr = OtherErr String
  deriving (Eq, Show)

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO

