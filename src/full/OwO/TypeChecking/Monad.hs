{-# LANGUAGE CPP #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map

import           OwO.Options
import           OwO.Syntax.Abstract

#include <impossible.h>

-- | Context
data TCEnv = TypeCheckingEnv
  { commandLineOptions :: CmdOptions
  -- ^ This is passed all around
  } deriving (Show)

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { commandLineOptions :: CmdOptions
  -- ^ This is passed all around
  } deriving Show

data TCErr = OtherErr String
  deriving (Eq, Show)

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO
