{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module OwO.Syntax.Abstract where

import qualified Data.Text           as T

import           OwO.Syntax.Common
import qualified OwO.Syntax.Concrete as C
import           OwO.Syntax.Position

import           GHC.Generics        (Generic)

data PsiTerm
  = PsiReference Loc QName
  -- ^ A reference to a variable
  {-
  | PsiLambda Loc QName Loc PsiTerm PsiTerm
  -- ^ Second interval is the name
  -}
  | PsiPatternVar Loc QName
  -- ^ Pattern variable
  | PsiImpossible Loc
  -- ^ Absurd pattern, impossible pattern
  | PsiDotPattern Loc PsiTerm
  -- ^ Dotted pattern
  | PsiMetaVar Loc QName
  -- ^ Meta variable
  deriving (Eq, Generic, Ord, Show)

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Generic, Ord, Show)

-- | Qualified Name
newtype QModuleName = QModuleName { moduleNameList :: [String] }
  deriving (Eq, Generic, Ord)

instance Show QModuleName where
  show (QModuleName ls) = concat $ ('.' :) <$> ls

-- | A name is a unique identifier and a suggestion for a concrete name. The
--   concrete name contains the source location (if any) of the name. The
--   source location of the binding site is also recorded.
data Name = Name
  { nameId         :: !NameId
  , nameConcrete   :: C.Name
  , nameBindingLoc :: Loc
  } deriving (Eq, Generic, Ord, Show)

mkName :: C.Name -> NameId -> Name
mkName name id = Name
  { nameId = id
  , nameConcrete = name
  , nameBindingLoc = C.locationOfName name
  }

-- | Qualified name, with module name
--   In definitions, the module name should be clear, so we add the module name
--   information here
data QName = QName
  { moduleName  :: QModuleName
  , concretName :: Name
  } deriving (Eq, Generic, Ord, Show)

simpleQName :: QName -> T.Text
simpleQName = C.textOfName . nameConcrete . concretName

simpleName :: Name -> T.Text
simpleName = C.textOfName . nameConcrete

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  } deriving (Eq, Generic, Ord, Show)

data PsiFixityInfo
  = PsiInfix
  | PsiInfixL
  | PsiInfixR
  deriving (Eq, Generic, Ord, Show)

-- | Function level pragma
data FnPragma
  = NonTerminate
  -- ^ Do not reduce, disable termination check
  | Instance
  -- ^ Add to instance search
  | Failing
  -- ^ Supposed to raise an error (Maybe specify the error type?)
  | Terminating
  -- ^ Disable termination check, unsafe
  deriving (Eq, Generic, Ord, Show)

type FnPragmas = [FnPragma]

data PsiDataCons' t = PsiDataCons
  { dataConsName :: QName
  , dataConsLoc  :: Loc
  , dataConsBody :: t
  } deriving (Eq, Functor, Generic, Ord, Show)

-- | Inductive data family
data PsiDataInfo' t
  = PsiDataDefinition
    { dataName     :: QName
    , dataNameLoc  :: Loc
    , dataTypeCons :: t
    , dataCons     :: [PsiDataCons' t]
    }
  -- ^ An in-place definition
  | PsiDataSignature
    { dataName     :: QName
    , dataNameLoc  :: Loc
    , dataTypeCons :: t
    } deriving (Eq, Functor, Generic, Ord, Show)
  -- ^ A data type signature, for mutual recursion

-- | One clause of a top-level definition. Term arguments to constructors are:
--
-- 1. The whole application (missing for PClauseR and PWithR because they're
-- within a "with" clause)
-- 2. The list of extra 'with' patterns
-- 3. The right-hand side
-- 4. The where block (PDecl' t)
data PsiPatternInfo' t
  = PsiPatternSimple  Loc QName t [t] t [PsiDeclaration' t]
  -- ^ Most simple pattern
  -- TODO
  {-
  | PsiPatternWith    Loc QName t [t] t [PsiDeclaration' t]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  | PsiPatternSimpleR Loc         [t] t [PsiDeclaration' t]
  -- ^ Most simple pattern
  | PsiPatternWithR   Loc         [t] t [PsiDeclaration' t]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  -}
  deriving (Eq, Generic, Functor, Ord, Show)

data DataPragma
  = NoPositivityCheck
  deriving (Eq, Generic, Ord, Show)

type DataPragmas = [DataPragma]

-- | Top-level declarations
--   TODOs: PsiCodata, PsiPattern, PsiCopattern
data PsiDeclaration' t
  = PsiFixity Loc PsiFixityInfo [QName]
  -- ^ infix, infixl, infixr
  | PsiType Loc QName FnPragmas t
  -- ^ Type signature
  | PsiSubmodule Loc QModuleName [PsiDeclaration' t]
  -- ^ Module defined in modules
  | PsiPostulate Loc QName FnPragmas t
  -- ^ Postulate, unsafe
  | PsiPrimitive Loc QName t
  -- ^ Primitive
  | PsiData Loc QName DataPragmas (PsiDataInfo' t)
  -- ^ Inductive data families
  | PsiPattern Loc FnPragmas [PsiPatternInfo' t]
  -- ^ Inductive data families
  deriving (Eq, Functor, Generic, Ord, Show)

type PsiDeclaration = PsiDeclaration' PsiTerm
type PsiDataCons    = PsiDataCons' PsiTerm
type PsiDataInfo    = PsiDataInfo' PsiTerm
type PsiPatternInfo = PsiPatternInfo' PsiTerm
