{-# LANGUAGE DeriveGeneric #-}

module OwO.Syntax.TokenType where

import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

data TokenType
  = InaccessiblePatternLToken
  -- ^ Dot pattern left
  | InaccessiblePatternRToken
  -- ^ Dot pattern right
  | ModuleToken
  | WhereToken
  | OpenToken
  | ImportToken
  | BracketLToken
  | BracketRToken
  -- ^ List literal
  | ParenthesisLToken
  | ParenthesisRToken
  | IdentifierToken
  | EndOfFileToken
  | LayoutEndToken
  -- ^ Finish a layout
  | ColonToken
  | NumberToken
  | LeftArrowToken
  -- ^ <-
  | RightArrowToken
  -- ^ ->
  | BraceLToken
  -- ^ {
  | BraceRToken
  -- ^ }
  | DataTypeToken
  | CodataTypeToken
  deriving (Eq, Generic, Ord, Show)

data PsiToken = PsiToken
  { tokenType :: TokenType
  , location  :: Loc
  } deriving (Eq, Generic, Ord, Show)

data LayoutContext
  = NoLayout
  | Layout Int
  deriving (Eq, Generic, Ord, Show)

-- | See @OwO.Syntax.Position@
data AlexUserState = AlexUserState
  { layoutStack     :: [LayoutContext]
  , currentFile     :: SrcFile
  , currentPosition :: !PositionNoFile
  , alexStartCodes  :: [Int]
  } deriving (Eq, Generic, Show)

-- | See @OwO.Syntax.Position@
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { layoutStack     = []
  , currentFile     = Strict.Nothing
  , currentPosition = emptyPosition
  , alexStartCodes  = []
  }
