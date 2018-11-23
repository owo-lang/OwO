{
{-# LANGUAGE LambdaCase #-}

module OwO.Syntax.Parser.Lexer where

import OwO.Syntax.TokenType
import OwO.Syntax.Position

import qualified Data.Text            as T
import qualified OwO.Util.StrictMaybe as Strict
}

%wrapper "monadUserState"

$digit       = [0-9]
$white_no_nl = $white # \n

@integer     = $digit+
@identifier  = [A-Za-z][0-9A-Za-z'_]*

tokens :-

$white_no_nl  ;
module        { simple ModuleToken }
open          { simple OpenToken }
data          { simple DataToken }
codata        { simple CodataToken }
import        { simple ImportToken }
where         { newLayoutContext >> simple WhereToken }
postulate     { newLayoutContext >> simple PostulateToken }
infix         { simple InfixToken }
infixl        { simple InfixLToken }
infixr        { simple InfixRToken }
@integer      { simpleString (IntegerToken . read) }
@identifier   { simpleString (IdentifierToken . T.pack) }
\<\-          { simple LeftArrowToken }
\-\>          { simple RightArrowToken }
\:            { simple ColonToken }
\[\|          { simple InaccessiblePatternLToken }
\|\]          { simple InaccessiblePatternRToken }
\(            { simple ParenthesisLToken }
\)            { simple ParenthesisRToken }
\{            { simple BraceLToken }
\}            { simple BraceRToken }
\[            { simple BracketLToken }
\]            { simple BracketRToken }
\=            { simple EqualToken }
\.            { simple DotToken }

<layout> {
  \n          ;
  ()          { newLayoutContext }
}

<0> {
  \n          { beginCode bol }
  where       { simple WhereToken }
}

<bol> {
  \n          ;
  ()          { doBol }
}

{

beginCode :: Int -> AlexAction PsiToken
beginCode n _ _ = do
  pushLexState n
  alexMonadScan

simple :: TokenType -> AlexAction PsiToken
simple t ((AlexPn pos line col), _, _, _) size = do
  file <- currentFile <$> alexGetUserState
  let start = simplePosition (pos - size) line (col - size)
  let end   = simplePosition pos line col
  pure $ PsiToken
    { tokenType = t
    , location  = locationFromSegment start end file
    }

simpleString :: (String -> TokenType) -> AlexAction PsiToken
simpleString f ((AlexPn pos line col), _, _, s) size =
   toMonadPsi pos line col size . f $ take size s

toMonadPsi :: Int -> Int -> Int -> Int -> TokenType -> Alex PsiToken
toMonadPsi pos line col size token = do
  file <- currentFile <$> alexGetUserState
  let start = simplePosition (pos - size) line (col - size)
  let end   = simplePosition pos line col
  pure $ PsiToken
    { tokenType = token
    , location  = locationFromSegment start end file
    }

alexEOF :: Alex PsiToken
alexEOF = getLayout >>= \case
    Nothing         -> java
    Just (Layout _) -> java
    Just  NoLayout  -> popLayout >> alexMonadScan
  where
    java = do
       (AlexPn pos line col, _, _, _) <- alexGetInput
       toMonadPsi pos line col 0 EndOfLayoutToken

doBol :: AlexAction PsiToken
doBol ((AlexPn pos line col), _, _, _) size =
  getLayout >>= \case
    Just (Layout n) -> case col `compare` n of
      LT -> popLayout >> pure EndOfLayoutToken >>= addToken
      EQ -> popLexState >> pure EndOfDirectiveToken >>= addToken
      GT -> popLexState >> alexMonadScan
    _ -> popLexState >> alexMonadScan
  where
    addToken = toMonadPsi pos line col size

newLayoutContext :: AlexAction PsiToken
newLayoutContext ((AlexPn pos line col), _, _, _) size = do
  popLexState
  pushLayout $ Layout col
  toMonadPsi pos line col size BeginOfLayoutToken

pushLayout :: LayoutContext -> Alex ()
pushLayout lc = do
  s@AlexUserState { layoutStack = lcs } <- alexGetUserState
  alexSetUserState s { layoutStack = lc : lcs }

popLayout :: Alex LayoutContext
popLayout = do
  s@AlexUserState { layoutStack = lcs } <- alexGetUserState
  case lcs of
    []        -> alexError "Layout expected but no layout available"
    lc : lcs' -> do
      alexSetUserState s { layoutStack = lcs' }
      pure lc

getLayout :: Alex (Maybe LayoutContext)
getLayout = do
  AlexUserState { layoutStack = lcs } <- alexGetUserState
  case lcs of
    []     -> pure Nothing
    lc : _ -> pure (Just lc)

pushLexState :: Int -> Alex ()
pushLexState nsc = do
  sc <- alexGetStartCode
  s@AlexUserState { alexStartCodes = scs } <- alexGetUserState
  alexSetUserState s { alexStartCodes = sc : scs }
  alexSetStartCode nsc

popLexState :: Alex Int
popLexState = do
  csc <- alexGetStartCode
  s@AlexUserState { alexStartCodes = scs } <- alexGetUserState
  case scs of
    []        -> alexError "State code expected but no state code available"
    sc : scs' -> do
      alexSetUserState s { alexStartCodes = scs' }
      alexSetStartCode sc
      pure csc

}
