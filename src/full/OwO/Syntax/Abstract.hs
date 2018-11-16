{-# LANGUAGE TypeOperators #-}

module OwO.Syntax.Abstract where

import OwO.Syntax.Common
import OwO.Syntax.Position
import qualified OwO.Syntax.Concret as C

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Ord, Show)

-- | Qualified Name
data QModuleName' str
  = QLocalName str
  | str :.: QModuleName' str
  deriving (Eq, Ord, Show)

-- | Specialized to String for convenience
type QModuleName = QModuleName' String

-- | A name is a unique identifier and a suggestion for a concrete name. The
--   concrete name contains the source location (if any) of the name. The
--   source location of the binding site is also recorded.
data Name = Name
  { nameId          :: !NameId
  , nameConcrete    :: C.Name
  , nameBindingSite :: Range
  } deriving (Eq, Ord, Show)

data QName = QName
  { moduleName  :: QModuleName
  , concretName :: Name
  } deriving (Eq, Ord, Show)

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  } deriving (Eq, Ord, Show)

