-- utility functions for converting between various types
import DFA
import CFG


-- Convert a discrete finite automaton into a context free
-- grammar.  This is accomplished by:
-- 1. Creating a variable S_i in the CFG for each state q_i
--    in the DFA
-- 2. FOr each transition d(q_i,a)=q_j in the DFA, create a
--    rule S_i -> aS_j
-- 3. For each accepting state q_j, add a rule S_j -> epsilon
-- 4. for start state q_k, the start of the grammar is S_k

dfaToCFG :: DFA a -> CFG a
dfaToCFG dfa = CFG {
        start = DFA.start dfa,
        variables = DFA.states dfa,
        terminals = a,
        rules = getRules dfa
    }
    where getRules (DFA _ _ t _) = 

-- To convert an NFA to a DFA
--   1. Compute the closure of the start state of the NFA.  This will be the 
--      DFA start state.
--   2. Create an empty DFA and add the start state whose value is the
--      closure from 1.  This will be the initial DFA.
--   3. Invoke the fromNFA' helper method with the NFA, the initial
--      DFA from 2., and the DFA start state.
--   4. Renumber the states in the DFA from 3. so that they are not sets of
--      states.
nfaToDFA :: Ord a => NFA a -> DFA a
nfaToDFA nfa@(NFA states moves start ends) =
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


