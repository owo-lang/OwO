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

import           OwO.Options
import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import qualified OwO.Syntax.Concrete        as C
import           OwO.Syntax.Position
import           OwO.TypeChecking.Core

import           GHC.Generics               (Generic)

#include <impossible.h>

-- | Context
type TCCtx a = Map.Map QModuleName (Map.Map TextName a)

emptyCtx :: TCCtx a
emptyCtx = Map.empty

mapCtx :: (a -> b) -> TCCtx a -> TCCtx b
mapCtx = fmap . fmap

-- | Maybe useful for completion?
--   Dunno, LOL.
allNames :: TCCtx a -> [(QModuleName, TextName)]
allNames ctx = Map.toList ctx >>=
  \ (m, ns) -> (m,) <$> Map.keys ns

-- | Lookup a definition in a known module
lookupCtxWithName :: QModuleName -> TextName -> TCCtx a -> Maybe a
lookupCtxWithName currentModule name ctx =
  Map.lookup currentModule ctx >>= Map.lookup name

lookupCtx :: QName -> TCCtx a -> Maybe a
lookupCtx (QName currentModule name) =
  lookupCtxWithName currentModule $ simpleName name

-- | Overwriting
addDefinitionWithName :: QModuleName -> TextName -> a -> TCCtx a -> TCCtx a
addDefinitionWithName targetModule name a ctx = maybe ctx
  ((\ctx' -> Map.insert targetModule ctx' ctx) <$> Map.insert name a)
  (Map.lookup targetModule ctx)

addDefinition :: QName -> a -> TCCtx a -> TCCtx a
addDefinition (QName currentModule name) =
  addDefinitionWithName currentModule $ simpleName name

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { stateOptions :: CompilerOptions
  -- ^ This is passed all around
  } deriving (Generic, Show)

-- | TypeChecking Environment
data TCEnv = TypeCheckingEnv
  { envState        :: TCState
  -- ^ This is passed all around
  } deriving (Generic, Show)

newtype TCErr' t
  = OtherErr t
  deriving (Eq, Functor, Show)

-- | TypeChecking Error
type TCErr = TCErr' PsiTerm

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO
