{-# LANGUAGE DeriveGeneric #-}
module OwO.Syntax.TokenType where

import           GHC.Generics        (Generic)
import           OwO.Syntax.Position

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
  , currentLocation :: !Int
  , currentLine     :: !Int
  , currentColumn   :: !Int
  , alexStartCodes  :: [Int]
  } deriving (Eq, Generic, Show)

-- | See @OwO.Syntax.Position@
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { layoutStack     = []
  , currentLocation = 1
  , currentLine     = 1
  , currentColumn   = 1
  , alexStartCodes  = []
  }
