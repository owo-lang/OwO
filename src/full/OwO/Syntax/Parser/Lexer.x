{
module OwO.Syntax.Parser.Lexer where

import OwO.Syntax.TokenType
import OwO.Syntax.Position

import qualified OwO.Util.StrictMaybe as Strict
}

%wrapper "monadUserState"

$digit = [0-9]
$white_no_nl = $white # \n

@number               = $digit +
@identifier           = [A-Za-z][A-Za-z'_]*

tokens :-

$white_no_nl  { newLine >> skip }
module        { simple ModuleToken }
open          { simple OpenToken }
data          { simple DataToken }
codata        { simple CodataToken }
import        { simple ImportToken }
where         { newLayoutContext >> simple WhereToken }
postulate     { newLayoutContext >> simple PostulateToken }
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

<0> {
  \n          { beginCode bol }
}

<bol> {
  \n          { newLine >> skip }
}

{

beginCode :: Int -> AlexAction PsiToken
beginCode n _ _ = do
  pushLexState n
  alexMonadScan

simple :: TokenType -> AlexAction PsiToken
simple t _ _ = do
  state    <- alexGetUserState
  let file = currentFile state
  let pos  = currentPosition state
  pure  $ PsiToken
    { tokenType = t
    , location  = emptyLocationIn file
    }

newLine :: AlexAction ()
newLine _ _ = do
  s@AlexUserState { currentPosition = pos } <- alexGetUserState
  let newPos = pos { posLine = posLine pos + 1
                   , posPos  = posPos  pos + 1
                   , posCol  = 0
                   }
  alexSetUserState s { currentPosition = newPos }

alexEOF :: Alex PsiToken
alexEOF = do
  l <- getLayout
  case l of
    Just (Layout _) -> do
      alex     <- popLayout
      state    <- alexGetUserState
      let file = currentFile state
      pure     $ PsiToken
        { tokenType = LayoutEndToken
        -- TODO
        , location  = emptyLocationIn file
        }
    Just  NoLayout  -> do
      _ <- popLayout
      alexMonadScan
    Nothing -> do
      alex     <- popLayout
      state    <- alexGetUserState
      let file = currentFile state
      pure     $ PsiToken
        { tokenType = LayoutEndToken
        -- TODO
        , location  = emptyLocationIn file
        }

getOffset :: Alex Int
getOffset = do
  (AlexPn _ _ column, _, _, _) <- alexGetInput
  pure column

newLayoutContext :: AlexAction ()
newLayoutContext _ _ = do
  popLexState
  offset <- getOffset
  pushLayout $ Layout offset

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
