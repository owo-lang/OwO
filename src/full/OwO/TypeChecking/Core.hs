{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core where

import qualified Data.Text           as T

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Position

import           GHC.Generics        (Generic)

#include <impossible.h>

-- | Alias, for refactoring convenience
type TextName = T.Text

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

-- | i should be something like a @Term@
data BinderInfo i
  = LambdaBinder !i
  -- ^ Lambda abstraction, type
  {-
  | Pi !i
  -}
  | LetBinder !i i
  -- ^ Let binding, type and value
  | NLetBinder !i i
  -- ^ Intermediate value used for reduction
  deriving (Eq, Functor, Generic, Ord, Show)

data ConstInfo
  = IntConst Int
  | IntegerConst Integer
  | StringConst String
  | CharConst Char
  deriving (Eq, Generic, Ord, Show)

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application
  | Var !Int
  -- ^ A variable resolved with de bruijn index
  | Param NameType i (Term' i)
  -- ^ Named reference, might be external definitions
  | Bind i !(BinderInfo (Term' i)) (Term' i)
  -- ^ Name binding
  | TType ULevel
  -- ^ Type of Type, including type omega
  | Constant ConstInfo
  deriving (Eq, Functor, Generic, Ord, Show)

-- TODO

-- | Term should have a @TextName@ coming from the parser
type Term = Term' TextName
-- | Aha! Dependent type!
type Type = Term

data Definition
  = SimpleDefinition !Type !Term
  deriving (Eq, Generic, Ord, Show)
