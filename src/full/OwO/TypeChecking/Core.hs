{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core where

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Position

import           GHC.Generics           (Generic)

#include <impossible.h>

data NameType
  = BoundName
  -- ^ Local name, should be already resolved.
  | FunctionName
  -- ^ Non-local name, should be already resolved.
  | TypeConstructor
  -- ^ Type constructor, should be already resolved.
  | DataConstructor
  -- ^ Data constructor, should be already resolved.
  deriving (Eq, Generic, Ord, Show)

data ULevel
  = ULevelLit Int
  -- ^ Like Type0, Type1
  | ULevelVar String Int
  -- ^ Level varaibles. Should be already computed.
  | ULevelMax
  -- TypeInf, TypeOmega
  deriving (Eq, Generic, Ord, Show)

-- TODO, something like a lambda/let/pattern/etc
data Binder i
  deriving (Eq, Functor, Generic, Ord, Show)

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application
  | Var !Int
  -- ^ A variable resolved with de bruijin index
  | P NameType i (Term' i)
  -- ^ Named reference, might be external definitions
  | Bind i !(Binder (Term' i)) (Term' i)
  -- ^ Name binding
  | TType ULevel
  -- ^ Type of Type, including type omega
  deriving (Eq, Functor, Generic, Ord, Show)

-- TODO

type Term = Term' QName
