module Main where

import NFA2
import qualified Data.Map as M
import qualified Data.Set as S
import System.Exit (exitSuccess, exitFailure)

invalidStartState :: S.Set Int
invalidStartState = S.singleton 4
validStartState :: S.Set Int
validStartState = S.singleton 3
validAcceptStates :: S.Set Int
validAcceptStates = S.fromList [1, 3]
invalidAcceptStates :: S.Set Int
invalidAcceptStates = S.fromList [3, 4]
validTransition :: M.Map (Int, Char) (S.Set Int)
validTransition = M.fromList [((1,'a'),S.singleton 1),((1,'b'),S.singleton 2),
                              ((2,'a'),S.singleton 1),((2,'b'),S.singleton 3),
                              ((3,'a'),S.singleton 1),((3,'b'), S.singleton 3)]
validString :: String
validString = "bbb"
invalidString :: String
invalidString = "bbab"


-- create returns a Just FA that contains the given transitions,
-- states, alphabet, and accepting states
testCreate :: Bool
testCreate = states fa == S.fromList (map fst (M.keys validTransition)) &&
             alphabet fa == S.fromList (map snd (M.keys validTransition)) &&
             transition fa == validTransition
    where (Just fa) = create validTransition validStartState validAcceptStates


-- accepts returns True for an empty sequence of symbols if the start state is
-- an accepting state, False otherwise.
testAcceptsEmptyAccepting :: Bool
testAcceptsEmptyAccepting = accepts fa "" where (Just fa) = create validTransition validStartState validAcceptStates
testRejectsEmptyNonaccepting :: Bool
testRejectsEmptyNonaccepting = not $ accepts fa ""
    where (Just fa) = create validTransition (S.singleton 2) validAcceptStates


-- accepts returns True if the FA should accept the sequence of symbols,
-- False otherwise.
testAcceptsValidString :: Bool
testAcceptsValidString = accepts fa validString
    where (Just fa) = create validTransition validStartState validAcceptStates
testRejectsInvalidString :: Bool
testRejectsInvalidString = not $ accepts fa invalidString
    where (Just fa) = create validTransition validStartState validAcceptStates


tests :: [Bool]
tests = [
    testAcceptsEmptyAccepting,
    testRejectsEmptyNonaccepting,
    testAcceptsValidString,
    testCreate]

 
main :: IO ()
main = if result then exitSuccess else do {print tests; exitFailure;}
    where result = and tests
