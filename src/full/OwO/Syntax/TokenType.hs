{-# LANGUAGE DeriveGeneric #-}

module OwO.Syntax.TokenType
 ( LayoutContext(..)

 -- Tokens
 , TokenType(..)
 , PsiToken(..)
 , isStartingNewLayout

 -- Alex
 , AlexUserState(..)
 , alexInitUserState
 ) where

import           Data.Text            as T

import           OwO.Syntax.Position
import qualified OwO.Util.StrictMaybe as Strict

import           GHC.Generics         (Generic)

data TokenType
  = ModuleToken
  -- ^ module
  | OpenToken
  -- ^ open
  | ImportToken
  -- ^ import
  | DataToken
  -- ^ data
  | CodataToken
  -- ^ codata
  | CaseToken
  -- ^ case (with abstraction)
  | CocaseToken
  -- ^ cocase (record constructor, coinductive)

  | InfixToken
  | InfixLToken
  | InfixRToken

  | WhereToken
  -- ^ where, starting a new layout
  | InstanceToken
  -- ^ instance, starting a new layout
  | PostulateToken
  -- ^ postulate, starting a new layout
  | OfToken
  -- ^ of (case), starting a new layout
  | DoToken
  -- ^ do, starting a new layout

  | BracketLToken
  -- ^ ], for `List` literal
  | BracketRToken
  -- ^ [, for `List` literal
  | ParenthesisLToken
  -- ^ (
  | ParenthesisRToken
  -- ^ )
  | SeparatorToken
  -- ^ |, with abstraction
  | IdiomBracketLToken
  -- ^ (|, idiom bracket
  | IdiomBracketRToken
  -- ^ |), idiom bracket
  | InstanceArgumentLToken
  -- ^ {|, instance argument
  | InstanceArgumentRToken
  -- ^ |}, instance argument
  | InaccessiblePatternLToken
  -- ^ [|, dot pattern
  | InaccessiblePatternRToken
  -- ^ |], dot pattern
  | ColonToken
  -- ^ :
  | LeftArrowToken
  -- ^ <-
  | RightArrowToken
  -- ^ ->
  | BraceLToken
  -- ^ {, implicit arguments, starts a layout
  | BraceRToken
  -- ^ }, finishes a layout
  | SemicolonToken
  -- ^ ;, finishes a line
  | EqualToken
  -- ^ =
  | DotToken
  -- ^ ., proof irrelevance, operators

  | IdentifierToken T.Text
  -- ^ identifier
  | OperatorToken T.Text
  -- ^ binary operators
  | StringToken T.Text
  -- ^ string literal
  | CharToken Char
  -- ^ character literal
  | IntegerToken Integer
  -- ^ integer numbers

  | EndOfFileToken
  -- ^ finishes a file
  deriving (Eq, Generic, Ord, Show)

isStartingNewLayout :: TokenType -> Bool
isStartingNewLayout WhereToken     = True
isStartingNewLayout PostulateToken = True
isStartingNewLayout InstanceToken  = True
isStartingNewLayout DoToken        = True
isStartingNewLayout OfToken        = True
isStartingNewLayout CocaseToken    = True
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
