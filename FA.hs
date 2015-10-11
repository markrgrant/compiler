-- A Finite Automaton abstract data type

module FA (
    create,
    accepts,
    states,
    alphabet,
    transition
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Applicative ((<$>), (<*>))


-- A transition is a mapping from (state, symbol) pairs to target states
type Transition a b = M.Map (a,b) a


-- a is the set of states
-- b is the set of transition symbols (the alphabet)
data FA a b = FA (Transition a b) a (S.Set a) deriving (Eq)


-- A finite automaton is a 3-tuple (transition, start, accept) where:
-- 1. transition is a mapping from a set of states and a set of symbols
--   (the alphabet) to another state
-- 2. start is a state from the set of states
-- 3. accept is a subset of the set of states
create :: (Ord a, Ord b) => Transition a b -> a -> S.Set a -> Maybe (FA a b)
create tr s ac 
    | not (S.member s st)= Nothing
    | not (S.isSubsetOf ac st) = Nothing
    | not (isDeterministic tr) = Nothing
    | otherwise = Just (FA tr s ac)
    where st = S.fromList $ map fst $ M.keys tr

isDeterministic :: (Eq a, Ord a, Eq b, Ord b) => Transition a b -> Bool
isDeterministic tr = domain == keys && S.isSubsetOf (S.fromList statesTo) (S.fromList statesFrom)
    where statesFrom = map fst $ M.keys tr
          symbols = map snd $ M.keys tr
          statesTo = M.elems tr
          domain = S.fromList $ (,) <$> statesFrom <*> symbols
          keys = S.fromList $ M.keys tr

-- A finite automata accepts a string if and only if there is a sequence of
-- states r such that the first state of the sequence is the start state, the
-- last is an accepting state, and for every symbol w_i in the string there
-- is a transition from state r_i to state r_(i+1).
accepts :: (Eq a, Ord a, Ord b) => FA a b -> [b] -> Bool
accepts fa@(FA _ s _) = accepts' fa s 

accepts' :: (Eq a, Ord a, Ord b) => FA a b -> a -> [b] -> Bool
accepts' (FA _ _ ac)  cur [] = S.member cur ac
accepts' fa@(FA t _ _) cur (x:xs) = accepts' fa (t M.! (cur, x)) xs

-- Returns the states of the FA
states :: (Ord a) => FA a b -> S.Set a
states (FA tr _ _) = S.fromList $ map fst $ M.keys tr

-- Returns the alphabet of the FA
alphabet :: (Ord b) => FA a b -> S.Set b
alphabet (FA tr _ _) = S.fromList $ map snd $ M.keys tr

-- Returns the transition map of the FA
transition :: () => FA a b -> M.Map (a,b) a
transition (FA tr _ _) = tr


-- TODO: define regular operations on FAs
-- union :: FA a b -> FA a b -> FA a b
-- intersection :: FA a b -> FA a b -> FA a b
-- star :: FA a b -> Int -> FA a b
