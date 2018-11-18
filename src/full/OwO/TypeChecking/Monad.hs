{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections         #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import           OwO.Options
import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import qualified OwO.Syntax.Concret         as C
import           OwO.Syntax.Position
import           OwO.TypeChecking.Core

import           GHC.Generics               (Generic)

#include <impossible.h>

-- | Alias, for refactoring convenience
type TextName = T.Text

-- | Context
type TCCtx a = Map.Map QModuleName (Map.Map TextName a)

emptyCtx :: TCCtx a
emptyCtx = Map.empty

-- | Maybe useful for completion?
--   Dunno, LOL.
allNames :: TCCtx a -> [(QModuleName, TextName)]
allNames ctx = Map.toList ctx >>=
  \ (m, ns) -> (m,) <$> Map.keys ns

lookupCtxWithName :: QModuleName -> TextName -> TCCtx a -> Maybe a
lookupCtxWithName currentModule name ctx =
  Map.lookup currentModule ctx >>= Map.lookup name

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
