{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}

module OwO.Syntax.Abstract where

import           OwO.Syntax.Common
import qualified OwO.Syntax.Concret  as C
import           OwO.Syntax.Position

data PsiTerm
  -- | A reference to a variable
  = PsiReference Loc QName
  {-
  -- | Second interval is the name
  | PsiLambda Loc QName Loc PsiTerm PsiTerm
  -}
  -- | Pattern variable
  | PsiPatternVar Loc QName
  -- | Absurd pattern, impossible pattern
  | PsiImpossible Loc
  -- | Dotted pattern
  | PsiDotPattern Loc PsiTerm
  -- | Meta variable
  | PsiMetaVar Loc QName
  deriving (Eq, Ord, Show)

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
  , nameBindingSite :: Loc
  } deriving (Eq, Ord, Show)

-- | Qualified name, with module name
data QName = QName
  { moduleName  :: QModuleName
  , concretName :: Name
  } deriving (Eq, Ord, Show)

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  } deriving (Eq, Ord, Show)

type Fixity = Int

-- | Function level pragma
data FnPragma
  -- | Do not reduce, disable termination check
  = NonTerminate
  -- | Add to instance search
  | Instance
  -- | Supposed to raise an error (Maybe specify the error type?)
  | Failing
  -- | Disable termination check, unsafe
  | Terminating
  deriving (Eq, Ord, Show)

type FnPragmas = [FnPragma]

data PsiDataCons' t = PsiDataCons
  { dataConsName :: QName
  , dataConsLoc  :: Loc
  , dataConsBody :: t
  } deriving (Eq, Functor, Show)

-- | Inductive data family
data PsiDataInfo' t
  -- | An in-place definition
  = PsiDataDefinition
    { dataName     :: QName
    , dataNameLoc  :: Loc
    , dataTypeCons :: t
    , dataCons     :: [PsiDataCons' t]
    }
  -- | A data type signature, for mutual recursion
  | PsiDataSignature
    { dataName     :: QName
    , dataNameLoc  :: Loc
    , dataTypeCons :: t
    } deriving (Eq, Functor, Show)

data DataPragma
  = NoPositivityCheck
  deriving (Eq, Ord, Show)

type DataPragmas = [DataPragma]

-- | Top-level declarations
--   TODOs: PsiCodata, PsiPattern, PsiCopattern
data PsiDeclaration' t
  -- | infix, infixl, infixr
  = PsiFixity Loc Fixity [QName]
  -- | Type signature
  | PsiType Loc QName FnPragmas t
  -- | Module defined in modules
  | PsiSubmodule Loc QModuleName [PsiDeclaration' t]
  -- | Postulate, unsafe
  | PsiPostulate Loc QName FnPragmas t
  -- | Primitive
  | PsiPrimitive Loc QName t
  -- | Inductive data families
  | PsiData Loc QName DataPragmas (PsiDataInfo' t)
  deriving (Eq, Functor, Ord, Show)

type PsiDeclaration  = PsiDeclaration' PsiTerm
type PsiDataCons    = PsiDataCons' PsiTerm
type PsiDataInfo    = PsiDataInfo' PsiTerm
