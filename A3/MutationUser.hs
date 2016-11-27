{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

module MutationUser(pointerTest, swap, swapCycle) where

import Mutation (
    get, set, def, Mutable, Pointer, Memory, StateOp(..), Value(..), (>>>), (>~>), returnVal, runOp
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp ((Pointer Integer, Pointer Bool))
pointerTest int =
    def 100 (int + 3) >~> \pt1 ->
    def 500 (int > 0) >~> \pt2 -> returnVal (pt1, pt2)

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap pt1 pt2 =
  get pt1 >~> \x1 ->
  get pt2 >~> \x2 ->
  set pt1 x2 >>>
  set pt2 x1

swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle (x:[]) = (swap x x)
swapCycle (x:y:[]) = (swap x y)
swapCycle (x:y:xs) = (swap x y) >>> (swapCycle (y:xs))
