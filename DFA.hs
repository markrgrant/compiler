-- A deterministic finite automaton (DFA) is a finite automata with no
-- epsilon moves, and at most one transition to a different state 
-- labeled with a given symbol.  As a consequence, a DFA can be in 
-- at most one state no matter the input. 
module DFA (
    fromNFA
) where

import qualified Set as S
import NFA
import Data.Maybe
import Data.List

data DFA a = DFA (S.Set a) (S.Set (Move a)) a (S.Set a) deriving (Eq, Show)

-- To convert an NFA to a DFA
--   1. Compute the closure of the start state of the NFA.  This will be the 
--      DFA start state.
--   2. Create an empty DFA and add the start state whose value is the
--      closure from 1.  This will be the initial DFA.
--   3. Invoke the fromNFA' helper method with the NFA, the initial
--      DFA from 2., and the DFA start state.
--   4. Renumber the states in the DFA from 3. so that they are not sets of
--      states.
fromNFA :: Ord a => NFA a -> DFA a
fromNFA nfa@(NFA states moves start ends) =
    number $ fromNFA' nfa dfa dfaStart
    where
        dfaStart = closure nfa (S.fromList [start])
        dfa = DFA (S.sing dfaStart) S.empty dfaStart S.empty
-- Convert the DFA whose states are sets of values to a DFA where each
-- state is a single distinct value.
number :: Ord a => DFA (S.Set a) -> DFA a
number (DFA oldstates oldmoves oldstart oldends) =
    DFA newstates newmoves newstart newends
    where
        newstates = foldr (\set newset -> S.union newset set) S.empty (S.toList oldstates)
        statemap = [pair|pair <- zip (S.toList oldstates) (S.toList newstates)]
        lookups = fromJust . ((flip lookup) statemap)
        newmoves = S.fromList $ map (\(Move start ch end) -> Move (lookups start) ch (lookups end)) (S.toList oldmoves)
        newstart = lookups oldstart
        newends =  S.fromList (map lookups (S.toList oldends))

-- Given an NFA, a DFA, and the next state from the DFA to be examined:
--   1. Get all of the Moves from the next state.  The end state of the Move must
--      be a closure.
--   2. For each Move:
--      3. If the Move's target is already in the DFA, just add the Move to the DFA.
--         Otherwise, Add the Move, add the Target state to the DFA, and call this
--         function again on the resulting DFA and the Target state.
fromNFA' :: Ord a => NFA a -> DFA (S.Set a) -> S.Set a -> DFA (S.Set a)
fromNFA' nfa dfa start =
    foldr (addmove nfa) dfa moves where moves = getmoves nfa start

-- Add the Move to the DFA.  If the target State is not yet in the DFA, call
-- the fromNFA' function again on the resulting DFA and State.
addmove :: Ord a => NFA a -> Move (S.Set a) -> DFA (S.Set a) -> DFA (S.Set a)
addmove nfa@(NFA nstates nmoves nstart nends)  dmove@(Move fr ch to) (DFA dstates dmoves dstart dends)
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
getmoves :: Ord a => NFA a -> (S.Set a) -> [Move (S.Set a)]
getmoves nfa@(NFA nstates nmoves nstart nends) dstart = closuremoves
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
-- optimize :: DFA a -> DFA a
-- optimize dfa = undefined
