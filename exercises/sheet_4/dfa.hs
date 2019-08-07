module DFA where

import Prelude hiding (Word)

type State = Int
type Alphabet a = [a]
type DFA a =
  ( Alphabet a             -- alphabet
  , State                  -- initial state
  , State -> a -> State    -- transition function
  , State -> Bool)         -- test for final state
type Word a = [a]

alphabet :: DFA a -> Alphabet a
alphabet (a, _, _, _) = a;

initial :: DFA a -> State
initial (_ , i, _, _) = i;

transition :: DFA a -> (State -> a -> State)
transition (_, _, t, _) = t

finalState :: DFA a -> State -> Bool
finalState (_, _, _, f) = f

accepts :: DFA a -> Word a -> Bool
accepts a w = finalState a $ foldr (\c acc -> (transition a) acc c) (initial a) w

lexicon :: Alphabet a -> Int -> [Word a]
lexicon a 0 = [[]]
lexicon a n = [ part ++ [l] | l <- a, part <- lexicon a (n - 1)]

language :: DFA a -> Int -> [Word a]
language a n = filter (accepts a) $ lexicon (alphabet a) n
