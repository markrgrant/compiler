-- The NFA module provides a data type representing nondeterministic
-- finite automata and methods for constructing an applying strings
-- to these automata.

module NFA where

import qualified Set as S
import RegExp

data NFA a = NFA (S.Set a) (S.Set (Move a)) a (S.Set a) deriving (Eq, Show)

data Move a = Move a Char a | Emove a a deriving (Eq, Ord, Show)


-- Given a set of States, find the set of states accessible by a Move on a
-- particular symbol in a given NFA.  Emoves are not included.
onemove :: Ord a => NFA a -> Char -> S.Set a -> S.Set a
onemove (NFA _ moves _ _) ch sources =
    S.fromList [to| Move fr c to <- (S.toList moves), S.member fr sources, c == ch]

-- Given an NFA and a set of states find the full set of states accessible from
-- the states by zero or more Emoves. This requires avoiding cycles.  The
-- original set of states will be contained in this full set of states
-- (and possibly equal to it).
closure :: Ord a => NFA a -> S.Set a -> S.Set a
closure nfa sources =
    S.setlimit (oneEmove nfa) sources

oneEmove (NFA _ moves _ _) sources =
    S.union (S.fromList [to| Emove fr to <- (S.toList moves), S.member fr sources]) sources


onetrans :: Ord a => NFA a -> Char -> S.Set a -> S.Set a
onetrans nfa ch fr = closure nfa (onemove nfa ch fr)

-- return the set of states which the nondeterministic finite automata
-- could be in after consuming the entire string.  The string is consumed
-- from left to right.  If there are no acceptable states (i.e. the NFA
-- rejects the string) the set returned will be empty.
trans :: Ord a => NFA a -> String -> S.Set a
trans nfa@(NFA _ _ start _) str = foldl step startset str
    where
        step acc ch = onetrans nfa ch acc
        startset = closure nfa (S.sing start)

-- Construct an NDA from a RegExp.
build :: RegExp -> NFA Int
build rexp = build' rexp 0

build' Epsilon start =
    NFA (S.fromList [start, end])
        (S.fromList [Emove start end])
        start
        (S.fromList [end])
    where end = start + 1 

build' (Literal c) start =
    NFA (S.fromList [start, end])
        (S.fromList [Move start c end])
        start
        (S.fromList [end])
    where end = start + 1

-- combine two NFAs into a single NFA that is the union of the two NFAs.
build' (Union r1 r2) start =
    NFA states
        moves
        start
        ends
    where
        (NFA states1 moves1 start1 ends1) = build' r1 (start + 1)
        (NFA states2 moves2 start2 ends2) = build' r2 (head (S.toList ends1) + 1)
        end = (head (S.toList ends2)) + 1
        ends = S.fromList [end]
        states = S.fromList ([start] ++ S.toList states1 ++ S.toList states2 ++ [end])
        moves = S.fromList ([Emove start start1] ++ [Emove start start2] ++ S.toList moves1 ++ S.toList moves2 ++ [Emove (head (S.toList ends1)) end] ++ [Emove (head (S.toList ends2)) end])
          
build' (Concat r1 r2) start = 
    NFA states
        moves
        start
        ends
    where
        (NFA states1 moves1 start1 ends1) = build' r1 (start + 1)
        (NFA states2 moves2 start2 ends2) = build' r2 (head (S.toList ends1) + 1)
        end1 = (head (S.toList ends1))
        end2 = (head (S.toList ends2))
        end = (head (S.toList ends2)) + 1
        ends = S.fromList [end]
        states = S.fromList ([start] ++ S.toList states1 ++ S.toList states2 ++ [end])
        moves = S.fromList ([Emove start start1] ++ [Emove end1 start2] ++ S.toList moves1 ++ S.toList moves2 ++ [Emove end2 end])

build' (Star r) start =
    NFA states
        moves
        start
        ends
    where
        (NFA states1 moves1 start1 ends1) = build' r (start + 1)
        end1 = (head (S.toList ends1))
        end = end1 + 1
        ends = S.fromList [end]
        states = S.fromList ([start] ++ S.toList states1 ++ [end])
        moves = S.fromList ([Emove start start1] ++ S.toList moves1 ++ [Emove end1 end] ++ [Emove end1 start1])
