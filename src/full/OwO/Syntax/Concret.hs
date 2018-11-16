{-# LANGUAGE CPP #-}
module OwO.Syntax.Concret where

import OwO.Syntax.Common
import OwO.Syntax.Position

#include <impossible.h>

{-| A name is a non-empty list of alternating 'Id's and 'Hole's. A normal name
    is represented by a singleton list, and operators are represented by a list
    with 'Hole's where the arguments should go. For instance: @[Hole,Id "+",Hole]@
    is infix addition.

    Equality and ordering on @Name@s are defined to ignore range so same names
    in different locations are equal.
-}
data Name
  = Name Range String   -- ^ A identifier.
  | NoName Range NameId -- ^ @_@.
  deriving (Eq, Ord, Show)
