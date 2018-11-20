{
module OwO.Syntax.Lexer

import OwO.Syntax.TokenType
}

%wrapper "monadUserState"

@inaccessiblePatternL = \{\|
@inaccessiblePatternR = \|\}
@module               = module
@where                = where
@identifier           = [A-Za-z][A-Za-z'_]*

{
}
