-- A nondeterministic finite automaton abstract data type

module NFA2 (
    create,
    accepts,
    states, 
    alphabet,
    transition
) where

import qualified Data.Set as S
import qualified Data.Map as M

type Transition a b = M.Map (a, b) (States a)

type States a = S.Set a

data NFA a b = NFA (Transition a b) (States a) (States a)

-- Create a valid nondeterministic finite automaton or Nothing if the arguments
-- are invalid.
-- TODO: Are there nondeterministic transition functions that can be invalid? 
create :: (Ord a) => Transition a b -> States a -> States a -> Maybe (NFA a b)
create tr start accept = Just (NFA tr start accept)
--    | not (isValidTransition tr) = Nothing
--    | otherwise = Just (NFA tr start accept)


-- A nondeterministic finite automata accepts a string if, after
-- processing each symbol in the string, there is an accepting
-- state in the possible accepting states.
accepts :: (Eq a, Ord a, Ord b) => NFA a b -> [b] -> Bool
accepts nfa@(NFA _ start _) = accepts' nfa start


accepts' :: (Eq a, Ord a, Ord b) => NFA a b -> States a -> [b] -> Bool
-- if the set of current states doesn't contain an accepting state after consuming
-- the string, then reject.  Otherwise accept.  
accepts' (NFA _ _ ac) cur [] = not (S.null (S.intersection cur ac))


-- get the next symbol, and for each state find the all next available states
-- and combine these to get the list of possible states and continue
accepts' nfa@(NFA t _ _) cur (x:xs) = accepts' nfa nextStates xs
   where nextStates =  S.foldl' (\acc st-> S.union acc (t M.! (st, x))) S.empty cur


-- Returns the states of the NFA
states :: (Ord a) => NFA a b -> States a
states (NFA tr _ _) = S.fromList $ map fst $ M.keys tr


-- Returns the alphabet of the NFA
alphabet :: (Ord b) => NFA a b -> S.Set b
alphabet (NFA tr _ _) = S.fromList $ map snd $ M.keys tr


-- Returns the transition map of the NFA
transition :: NFA a b -> M.Map (a, b) (States a)
transition (NFA tr _ _) = tr
