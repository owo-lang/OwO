{-# LANGUAGE CPP #-}
module OwO.Syntax.Common where

import Data.Word

#include <impossible.h>

---------------------------------------------------------------------------
-- * NameId
---------------------------------------------------------------------------

-- | The unique identifier of a name. Second argument is the top-level module
--   identifier.
data NameId = NameId {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
    deriving (Eq, Ord)

instance Show NameId where
  show (NameId n m) = show n ++ "@" ++ show m

instance Enum NameId where
  succ (NameId n m)     = NameId (n + 1) m
  pred (NameId n m)     = NameId (n - 1) m
  toEnum n              = __IMPOSSIBLE__ -- ^ should not be used
  fromEnum (NameId n _) = fromIntegral n

---------------------------------------------------------------------------
-- * Meta variables
---------------------------------------------------------------------------

-- | A meta variable identifier is just a natural number.
--   It can have a name, as in Idris.
--
data MetaId = MetaId
  { metaId   :: Int
  , metaName :: Maybe Word
  } deriving (Eq, Ord)

-- | Show non-record version of this newtype.
instance Show MetaId where
  showsPrec p (MetaId n m) = showParen (p > 0) $
    showString "MetaId " . shows n
