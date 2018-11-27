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

-- | The type parameter c is the name representation, which is probably T.Text
data PsiTerm' c
  = PsiReference Loc c
  -- ^ A reference to a variable
  {-
  | PsiLambda Loc c Loc (PsiTerm' c) (PsiTerm' c)
  -- ^ Second interval is the name
  -}
  | PsiPatternVar Loc c
  -- ^ Pattern variable
  | PsiImpossible Loc
  -- ^ Absurd pattern, impossible pattern
  | PsiDotPattern Loc (PsiTerm' c)
  -- ^ Dotted pattern
  | PsiMetaVar Loc c
  -- ^ Meta variable
  deriving (Eq, Generic, Ord, Show)

type PsiTerm = PsiTerm' T.Text

-- | Program Structure Item: File Type
data PsiFileType
  = CodeFileType
  | LiterateFileType
  deriving (Eq, Generic, Ord, Show)

-- | Qualified Name
newtype QModuleName = QModuleName { moduleNameList :: [String] }
  deriving (Eq, Generic, Ord)

hasParentModule :: QModuleName -> Bool
hasParentModule (QModuleName ls) = ls /= []

parentModule :: QModuleName -> Maybe QModuleName
parentModule (QModuleName list) = case list of
  [      ] -> Nothing
  (l : ls) -> Just $ QModuleName { moduleNameList = ls }

instance Show QModuleName where
  show (QModuleName ls) = concat $ ('.' :) <$> ls

-- | Qualified name, with module name
--   A name is a unique identifier and a suggestion for a concrete name. The
--   concrete name contains the source location (if any) of the name. The
--   source location of the binding site is also recorded.
--   In definitions, the module name should be clear, so we add the module name
--   information here
data QName = QName
  { nameModule     :: QModuleName
  , nameId         :: !NameId
  , nameConcrete   :: C.Name
  , nameBindingLoc :: Loc
  } deriving (Eq, Generic, Ord, Show)

simpleName :: QName -> T.Text
simpleName = C.textOfName . nameConcrete

-- | Program Structure Item: File
data PsiFile = PsiFile
  { fileType           :: PsiFileType
  , topLevelModuleName :: QModuleName
  , declarations       :: [PsiDeclaration]
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

data PsiDataCons' t c = PsiDataCons
  { dataConsName :: c
  , dataConsLoc  :: Loc
  , dataConsBody :: t c
  } deriving (Eq, Functor, Generic, Ord, Show)

-- | Inductive data family
data PsiDataInfo' t c
  = PsiDataDefinition
    { dataName     :: c
    , dataNameLoc  :: Loc
    , dataTypeCons :: t c
    , dataCons     :: [PsiDataCons' t c]
    }
  -- ^ An in-place definition
  | PsiDataSignature
    { dataName     :: c
    , dataNameLoc  :: Loc
    , dataTypeCons :: t c
    } deriving (Eq, Functor, Generic, Ord, Show)
  -- ^ A data type signature, for mutual recursion

-- | One clause of a top-level definition. Term arguments to constructors are:
--
-- 1. The whole application (missing for PClauseR and PWithR because they're
--    within a "with" clause)
-- 2. The list of extra 'with' patterns
-- 3. The right-hand side
-- 4. The where block (PsiDeclaration' t)
data PsiPatternInfo' t c
  = PsiPatternSimple  Loc c (t c) [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  -- TODO
  {-
  | PsiPatternWith    Loc c (t c) [t c] (t c) [PsiDeclaration' t c]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  | PsiPatternSimpleR Loc         [t c] (t c) [PsiDeclaration' t c]
  -- ^ Most simple pattern
  | PsiPatternWithR   Loc         [t c] (t c) [PsiDeclaration' t c]
  -- ^ Pattern with 'with', we may use the keyword 'case'
  -}
  deriving (Eq, Generic, Functor, Ord, Show)

data DataPragma
  = NoPositivityCheck
  deriving (Eq, Generic, Ord, Show)

type DataPragmas = [DataPragma]

-- | Top-level declarations
--   TODOs: PsiCodata, PsiPattern, PsiCopattern
data PsiDeclaration' t c
  = PsiFixity Loc PsiFixityInfo [c]
  -- ^ infix, infixl, infixr
  | PsiType Loc c FnPragmas (t c)
  -- ^ Type signature
  | PsiSubmodule Loc QModuleName [PsiDeclaration' t c]
  -- ^ Module defined in modules
  | PsiPostulate Loc c FnPragmas (t c)
  -- ^ Postulate, unsafe
  | PsiPrimitive Loc c (t c)
  -- ^ Primitive
  | PsiData Loc QName DataPragmas (PsiDataInfo' t c)
  -- ^ Inductive data families
  | PsiPattern Loc FnPragmas [PsiPatternInfo' t c]
  -- ^ A pattern matching clause
  deriving (Eq, Functor, Generic, Ord, Show)

type PsiDeclaration = PsiDeclaration' PsiTerm' T.Text
type PsiDataCons    = PsiDataCons'    PsiTerm' T.Text
type PsiDataInfo    = PsiDataInfo'    PsiTerm' T.Text
type PsiPatternInfo = PsiPatternInfo' PsiTerm' T.Text
