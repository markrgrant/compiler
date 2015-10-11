module Main where

import FA
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isNothing)
import System.Exit (exitSuccess, exitFailure)

invalidStartState :: Int
invalidStartState = 4
validStartState :: Int
validStartState = 3
validAcceptStates :: S.Set Int
validAcceptStates = S.fromList [1, 3]
invalidAcceptStates :: S.Set Int
invalidAcceptStates = S.fromList [3, 4]
invalidTransition :: M.Map (Int, Char) Int
invalidTransition = M.fromList [((1, 'a'), 2)]
validTransition :: M.Map (Int, Char) Int
validTransition = M.fromList [((1,'a'),1),((1,'b'),2),((2,'a'),1),((2,'b'),3),((3,'a'),1),((3,'b'),3)]
validString :: String
validString = "bbb"
invalidString :: String
invalidString = "bbab"

  
-- create returns Nothing if the start state is not a member of the states
testInvalidStartState :: Bool 
testInvalidStartState = isNothing (create validTransition invalidStartState validAcceptStates)


-- create returns Nothing if the accept states are not a subset of the states
testInvalidAcceptStates :: Bool
testInvalidAcceptStates = isNothing (create validTransition validStartState invalidAcceptStates)


-- create returns Nothing if the transition function is invalid
testInvalidTransition :: Bool
testInvalidTransition = isNothing (create invalidTransition validStartState invalidAcceptStates)


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
testRejectsEmptyNonaccepting = not $ accepts fa "" where (Just fa) = create validTransition 2 validAcceptStates


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
    testInvalidStartState,
    testInvalidAcceptStates,
    testInvalidTransition,
    testAcceptsEmptyAccepting,
    testRejectsEmptyNonaccepting,
    testAcceptsValidString,
    testCreate]

 
main :: IO ()
main = if result then exitSuccess else do {print tests; exitFailure;}
    where result = and tests
