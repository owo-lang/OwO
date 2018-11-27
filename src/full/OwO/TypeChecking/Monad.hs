{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections         #-}

-- | OwO's type checking state is a state monad transformer.
--   We call it TypeCheckingMonad, in short TCM, as Agda does.
module OwO.TypeChecking.Monad where

import           Control.Applicative        (Alternative (..), (<|>))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map
import qualified Data.Text                  as T

import           OwO.Options
import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import qualified OwO.Syntax.Concrete        as C
import           OwO.Syntax.Position
import           OwO.TypeChecking.Core

import           GHC.Generics               (Generic)

#include <impossible.h>

type CtxBindingKey = T.Text

-- | Context
type TCCtx a = Map.Map QModuleName (Map.Map CtxBindingKey a)

emptyCtx :: TCCtx a
emptyCtx = Map.empty

mapCtx :: (a -> b) -> TCCtx a -> TCCtx b
mapCtx = fmap . fmap

-- | Maybe useful for completion?
--   Dunno, LOL.
allNames :: TCCtx a -> [(QModuleName, CtxBindingKey)]
allNames ctx = Map.toList ctx >>=
  \ (m, ns) -> (m,) <$> Map.keys ns

-- | Lookup a definition in a known module
lookupCtxWithName :: QModuleName -> CtxBindingKey -> TCCtx a -> Maybe a
lookupCtxWithName currentModule name ctx =
  (Map.lookup currentModule ctx >>= Map.lookup name) <|>
  (parentModule currentModule >>= \m -> lookupCtxWithName m name ctx)

lookupCtx :: QName -> TCCtx a -> Maybe a
lookupCtx (QName currentModule _ name _) =
  lookupCtxWithName currentModule $ C.textOfName name

-- | Overwriting
addDefinitionWithName :: QModuleName -> CtxBindingKey -> a -> TCCtx a -> TCCtx a
addDefinitionWithName targetModule name a ctx = maybe ctx
  ((\ctx' -> Map.insert targetModule ctx' ctx) <$> Map.insert name a)
  (Map.lookup targetModule ctx)

addDefinition :: QName -> a -> TCCtx a -> TCCtx a
addDefinition (QName currentModule _ name _) =
  addDefinitionWithName currentModule $ C.textOfName name

-- | TypeChecking State. I haven't decide on whether to store warnings here
--   (but errors should definitely be in the other side of the Monad)
data TCState = TypeCheckingState
  { stateOptions     :: CompilerOptions
  -- ^ This is passed all around
  , stateDefinitions :: TCCtx Definition
  -- ^ A symbol table containing all type-checked definitions
  } deriving (Generic, Show)

emptyTCState :: CompilerOptions -> TCState
emptyTCState opts = TypeCheckingState
  { stateOptions     = opts
  , stateDefinitions = emptyCtx
  }

-- | TypeChecking Environment
data TCEnv = TypeCheckingEnv
  { envState       :: TCState
  -- ^ This is passed all around
  , envDefinitions :: TCCtx Definition
  -- ^ Local definitions
  } deriving (Generic, Show)

data TCErr' t
  = OtherErr t
  | UnresolvedReferenceErr C.Name
  deriving (Eq, Functor, Show)

-- | TypeChecking Error
type TCErr = TCErr' PsiTerm

-- | TypeChecking Monad Transformer
type TCMT m = StateT TCState (ExceptT TCErr m)

-- | TypeChecking Monad
type TCM = TCMT IO
