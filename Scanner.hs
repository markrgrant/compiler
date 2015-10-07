{-

A scanner takes a lexical specification and a string and returns a stream of tokens

-}


module Scanner where

import LexicalSpec

data RegExp = EpsilonExp | CharExp Char
type Token = (TokenClass, Lexeme)
type Lexeme = String
type LineNumbr = Int

data Operator = PlusOperator | MinusOperator | AssignmentOperator | EqualsOperator


-- break up source code into an ordered sequence of tokens
scan :: LexicalSpec -> String -> [Token]
scan source = words source
