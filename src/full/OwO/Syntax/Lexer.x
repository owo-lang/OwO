{
module OwO.Syntax.Lexer where

import OwO.Syntax.TokenType
}

%wrapper "monadUserState"

$digit = [0-9]
$white_no_nl = $white # \n

@inaccessiblePatternL = \{\|
@inaccessiblePatternR = \|\}
@braceL               = \{
@braceR               = \}
@parenthesisL         = \(
@parenthesisR         = \)
@leftArrow            = \<\-
@rightArrow           = \-\>
@colon                = :
@number               = $digit +
@module               = module
@where                = where
@identifier           = [A-Za-z][A-Za-z'_]*

tokens :-
  $white_no_nl+            ;

{

beginCode :: Int -> AlexAction PsiToken
beginCode n _ _ = do
  pushLexState n
  alexMonadScan

alexEOF :: Alex PsiToken
alexEOF = do
  l <- getLayout
  case l of
    Just (Layout _) -> do
      _ <- popLayout
      pure $ PsiToken
        { tokenType = LayoutEndToken
        , location  =
        }
    Just  NoLayout  -> do
      _ <- popLayout
      alexMonadScan
    Nothing -> pure $ PsiToken
      { tokenType = LayoutEndToken
      , location  = 
      }

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