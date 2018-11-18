{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Core language
module OwO.TypeChecking.Core where

import           OwO.Syntax.Abstract
import           OwO.Syntax.Common
import           OwO.Syntax.Position

import           GHC.Generics        (Generic)

#include <impossible.h>

-- | Core language term, @i@ refers to the identifier.
--   We translate type-checked code into this form
data Term' i
  = App !(Term' i) (Term' i)
  -- ^ Application
  | Var !Int
  -- ^ A variable resolved with de bruijin index
  deriving (Eq, Functor, Generic, Ord, Show)

-- TODO

type Term = Term' QName
