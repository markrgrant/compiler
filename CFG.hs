-- Types for modeling context-free grammars.  A context-free
-- grammar is a set of rules that define the valid
-- sentences in a language.  A sentence is a sequence
-- of words.
--
-- it is important that the grammar not be ambiguous (only
-- one parse tree should be possible for any expression)
-- 
-- There are several classes of Grammar
-- not-LR grammar - worst case O(n^3)
-- LR(1) grammar  - bottom up parsing, sufficient for all
--                  programming languages
-- LL(1) grammar  - bottom up or top down, most programming
--                  languages can be expressed as LL(1)
-- regular grammar

module CFG where


-- by defening a grammar in this way are we guaranteed
-- that it is context-free?  That would be ideal.
-- Note that the variables must equal the start variables
-- of the rules, and the set of terminals must equal the 
-- set of terminals found in the rules, and the variable
-- of the first rule in the list of rules must be the
-- start variable
data CFG a = CFG {
    start :: Variable,
    variables :: [Variable],
    terminals :: a,
    rules :: [Rule]
}

type Terminal = Char
data Symbol a = Terminal a | Variable a

-- a production rule with its variable symbol and
-- sequence of terminal or nonterminal symbols
data Rule = Rule Variable [Symbol]


-- a nonterminal symbol in the production rules
data Variable = Variable String


-- Lazily generate strings in the language of the grammar
language :: CFG -> [String]
language = undefined


-- returns True if th destination symbols list can
-- be achieved through application of grammar rules
-- to the source list.
derives :: Grammar -> [Symbol] -> [Symbol] -> Bool
derives grammar fr to [] =
    
    where rs = rules grammar


isLLGrammar :: CFG -> Bool
isLLGrammar = undefined

isLRGrammar :: CFG -> Bool
isLRGrammar = undefined


-- a grammaer is left recursive if it has a left recursive
-- rule.  A rule is left recursive if the first symbol
-- on its right hand side is the symbol on its left side
-- or is a downstream symbol of that right hand symbol.
isLeftRecursive :: CFG -> Bool
isLeftRecursive (CFG rules) = any isLeftRecursiveRule rules

isLeftRecursiveRule :: Rule -> Bool
isLeftRecursiveRule = undefined


-- left recursive grammars can recurse infinitely when
-- top down parsing is used.  This function takes a 
-- grammar (possibly left recursive) and returns a 
-- grammar that is not left recursive.  This is done by
-- 1. turning indirect left recursion into direct left recursion
-- 2. rewriting direct left recursion as right recursion
makeLeftRecursive :: CFG -> CFG
makeLeftRecursive cfg = undefined


isAmbiguous :: CFG a -> Bool
isAmbiguous = undefined


-- convert the CFG into Chomsky Normal Form where every
-- rule is of the form A->BC, A->a, or S->epsilon where
-- epsilon is the start state.
-- 1. 
toCNF :: CFG -> CFG
toCNF = addStart >>> removeEpsilons >>> removeUnits


-- add a new start variable so that it's guaranteed
-- not to be part of any other rule
addStart :: CFG -> CFG
addStart cfg {start=oldStart, rules=oldRules} =
    cfg {start=
