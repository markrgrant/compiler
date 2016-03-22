-- A compiler's parser has responsibility for
-- recognizing syntax of a programming language.
--
-- Programming language syntax is represented using a
-- context-free grammar.
--
-- parsing algorithms:
-- 1. top-down - match the input stream against the 
--    productions of the grammar by predicting the next
--    word.  This is efficient for only some grammars.
-- 2. bottom-up - starting with the sequence of words,
--    context is accumulated until the derivation is certain.
--    This is efficient for a larger class of grammars
--    than top-down parsing.
--
-- regular expressions are not sufficient models of
-- languages because they cannot model, for example,
-- languages with parentheses around expressions because
-- they can't count.
--


module Parser where


import CFG

type Parser = Grammar -> [Token] -> IR
type IR = IREmpty | IR SynRep [IR]


-- a parser suitable for parsing LR(1) and LL(1)
-- grammars
bottomUpParser :: Parser
bottomUpParser grammar tokens
    | empty tokens = IREmpty


-- a parser suitable for parsing LL(1) grammars.  It starts
-- with the start symbol of the grammar, and then finds a rule
-- given that symbol whose right hand side matches.
--
-- This parser is leftMost
topDownParser :: Parser
topDownParser grammar tokens = topDownParser' grammar tokens (startSymbol grammar)


-- top down parsing is complete when a token is rejected or
-- when the fringe of the intermediate representation is
-- 
topDownParser' grammar [] ir = ir
