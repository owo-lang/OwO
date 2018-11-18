{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map

import           OwO.Options
import           OwO.Syntax.Abstract
import           OwO.TypeChecking.Core

import           GHC.Generics               (Generic)

#include <impossible.h>

-- | Context
type TCCtx a = Map.Map QName a

-- | Maybe useful for completion?
--   Dunno, LOL.
allNames :: TCCtx a -> [QName]
allNames = Map.keys

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { stateOptions :: CmdOptions
  -- ^ This is passed all around
  } deriving (Generic, Show)

-- | TypeChecking Environment
data TCEnv = TypeCheckingEnv
  { envState        :: TCState
  -- ^ This is passed all around
  } deriving (Generic, Show)

data TCErr' t
  = OtherErr t
  deriving (Eq, Functor, Show)

type TCErr = TCErr' PsiTerm

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO
