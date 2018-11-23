{-# LANGUAGE DeriveGeneric #-}

module OwO.Syntax.TokenType where

import           Data.Text            as T

import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

data TokenType
  = InaccessiblePatternLToken
  -- ^ Dot pattern left
  | InaccessiblePatternRToken
  -- ^ Dot pattern right
  | ModuleToken
  -- ^ module
  | OpenToken
  -- ^ open
  | ImportToken
  -- ^ import
  | DataToken
  -- ^ data
  | CodataToken
  -- ^ codata

  | InfixToken
  | InfixLToken
  | InfixRToken

  | WhereToken
  -- ^ where, starting a new layout
  | PostulateToken
  -- ^ postulate, starting a new layout

  | BracketLToken
  -- ^ ], for `List` literal
  | BracketRToken
  -- ^ [, for `List` literal
  | ParenthesisLToken
  -- ^ (
  | ParenthesisRToken
  -- ^ )
  | ColonToken
  -- ^ :
  | LeftArrowToken
  -- ^ <-
  | RightArrowToken
  -- ^ ->
  | BraceLToken
  -- ^ {
  | BraceRToken
  -- ^ }
  | EqualToken
  -- ^ =
  | DotToken
  -- ^ .

  | IdentifierToken T.Text
  -- ^ identifier
  | IntegerToken Integer
  -- ^ integer numbers

  | BeginOfLayoutToken
  -- ^ starts a layout
  | EndOfLayoutToken
  -- ^ finishes a layout
  | EndOfDirectiveToken
  -- ^ finishes a line (like a semicolon)
  | EndOfFileToken
  -- ^ finishes a file
  deriving (Eq, Generic, Ord, Show)

isStartingNewLayout :: TokenType -> Bool
isStartingNewLayout WhereToken     = True
isStartingNewLayout PostulateToken = True
isStartingNewLayout _              = False

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
  { layoutStack    :: [LayoutContext]
  , currentFile    :: SrcFile
  , alexStartCodes :: [Int]
  } deriving (Eq, Generic, Show)

-- | See @OwO.Syntax.Position@
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { layoutStack    = []
  , currentFile    = Strict.Nothing
  , alexStartCodes = []
  }
