{
{-# LANGUAGE LambdaCase #-}

module OwO.Syntax.Parser.Lexer where

import OwO.Syntax.TokenType
import OwO.Syntax.Position

import qualified Data.Text            as T
import qualified OwO.Util.StrictMaybe as Strict
}

%wrapper "monadUserState"

$digit = [0-9]
$white_no_nl = $white # \n

@number               = $digit +
@identifier           = [A-Za-z][A-Za-z'_]*

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

<0> {
  \n          { beginCode bol }
}

<bol> {
  \n          ;
}

{

beginCode :: Int -> AlexAction PsiToken
beginCode n _ _ = do
  pushLexState n
  alexMonadScan

simple :: TokenType -> AlexAction PsiToken
simple t ((AlexPn _ _ col), _, _, _) _ = do
  file <- currentFile <$> alexGetUserState
  pure  $ PsiToken
    { tokenType = t
    , location  = emptyLocationIn file
    }

simpleString :: (T.Text -> a) -> AlexAction a
simpleString f (_, _, _, s) len = pure . f . T.pack $ take len s

alexEOF :: Alex PsiToken
alexEOF = getLayout >>= \case
    Nothing         -> java
    Just (Layout _) -> java
    Just  NoLayout  -> popLayout >> alexMonadScan
  where
    java = do
       (AlexPn pos line col, _, _, _) <- alexGetInput
       file <- currentFile <$> alexGetUserState
       let pwf = Position
             { srcFile = ()
             , posPos  = pos
             , posLine = line
             , posCol  = col
             }
       let pos = positionWithFile pwf file
       pure $ PsiToken
         { tokenType = LayoutEndToken
         , location  = Loc { iStart = pos, iEnd = pos }
         }

newLayoutContext :: AlexAction ()
newLayoutContext ((AlexPn _ _ col), _, _, _) _ = do
  popLexState
  pushLayout $ Layout col

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
