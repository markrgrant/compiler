-- A deterministic finite automaton (DFA) is a finite automata with no
-- epsilon moves, and at most one transition to a different state 
-- labeled with a given symbol.  As a consequence, a DFA can be in 
-- at most one state no matter the input. 
module DFA where

import qualified Set as S
import NFA
import Data.Maybe
import Data.List

data DFA a = DFA (S.Set a) (S.Set (Move a)) a (S.Set a) deriving (Eq, Show)

-- Add the Move to the DFA.  If the target State is not yet in the DFA, call
-- the fromNFA' function again on the resulting DFA and State.
addMove :: Ord a => NFA a -> Move (S.Set a) -> DFA (S.Set a) -> DFA (S.Set a)
addMove nfa@(NFA nstates nmoves nstart nends)  dmove@(Move fr ch to) (DFA dstates dmoves dstart dends)
    | S.member to dstates = (DFA dstates newdmoves dstart newdends)
    | otherwise = fromNFA' nfa newdfa to
    where
        newdstates = S.union dstates (S.sing to)
        newdmoves = S.union dmoves (S.sing dmove)
        newdends = if (S.diff to nends == S.empty) then dends else S.union dends (S.sing to)
        newdfa = (DFA newdstates newdmoves dstart newdends)

-- Given an NFA, and a set of states in the NFA:
--   1. Find all valid Moves from that set of states.
--   2. Group the destination states by their Move symbols.  --   3. Compute their epsilon closure.
--   4. For each symbol, create a new Move where the destination state is 3.
--   5. Return a list of these symbols
getMoves :: Ord a => NFA a -> (S.Set a) -> [Move (S.Set a)]
getMoves nfa@(NFA nstates nmoves nstart nends) dstart = closuremoves
   where startstates = (S.toList dstart)
        -- get the NDA moves from the DFAs start state
         isMoveFromStart (Move fr ch to) = S.member fr dstart
         isMoveFromStart (Emove _ _) = False
         groupedMoves = groupBy (\(Move fr1 ch1 to1) (Move fr2 ch2 to2) -> ch1 == ch2)
                                (filter isMoveFromStart (S.toList nmoves))
         makeMove moveList = foldr (\ (Move _ ch to) (Move dfr dch dto) -> (Move dfr ch (S.union dto (S.sing to))))
                                   (Move dstart 'n' S.empty)
                                   moveList
         moves = map makeMove groupedMoves
         closuremoves = map (\(Move fr ch to) -> (Move fr ch (closure nfa to))) moves

-- optimize a DFA so it contains the minimum number of 
-- possible states and still accepts the same language
-- as the original DFA
optimize :: DFA a -> DFA a
optimize dfa = undefined
