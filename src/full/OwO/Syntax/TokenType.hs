{-# LANGUAGE DeriveGeneric #-}
module OwO.Syntax.TokenType where

import           GHC.Generics        (Generic)
import           OwO.Syntax.Position

data TokenType
  = InaccessiblePatternLToken
  -- * Dot pattern left
  | InaccessiblePatternRToken
  -- * Dot pattern left
  | ModuleToken
  | WhereToken
  | BracketLToken
  -- ^ Lists
  | BracketRToken
  -- ^ Lists
  | ParenthesisLToken
  | ParenthesisRToken
  | IdentifierToken
  | EndOfFileToken
  deriving (Eq, Generic, Ord, Show)

data PsiToken = PsiToken
  { tokenType :: TokenType
  , location  :: Loc
  } deriving (Eq, Generic, Ord, Show)

data LayoutContext
  = NoLayout
  | Layout Int
  deriving (Eq, Generic, Ord, Show)
