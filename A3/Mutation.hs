{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..),
    (>>>), (>~>), StateOp(..), Value(..),
    returnVal, runOp, alloc, free
    )
    where

import AList (AList, lookupA, insertA, updateA)
import Data.List (sortBy, intersect, nub)
-- helpers to sort memory
compareFirst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFirst (x, _) (y, _) = compare y x

sortMem :: Memory -> Memory
sortMem mem = sortBy compareFirst mem

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show

--
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
f >~> g =
    StateOp(\mem ->
        let {(x, mem') = runOp f mem
            ;newStateOp = g x}
            in runOp newStateOp mem')

(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = StateOp (\m ->
                let (_, mem) = runOp op1 m
                in runOp op2 mem)

returnVal :: a -> StateOp a
returnVal val = StateOp (\mem -> (val, mem))

-- Example usage:
g :: Integer -> StateOp Integer
g x =
    def 1 (x + 4) >~> \p ->
    get p >~> \y ->
    returnVal (x*y)
-- test with runOp (g 10)[]
-- output should be : (140, [(1, IntVal 14)])

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    --get :: Memory -> Pointer a -> a
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    --set :: Memory -> Pointer a -> a -> Memory
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    --def :: Memory -> Integer -> a -> (Pointer a, Memory)
    def :: Integer -> a -> StateOp (Pointer a)

instance Mutable Integer where
    -- TODO: error checking

    --get memory (P pt) = case lookupA memory pt of IntVal x -> x
    --set memory (P pt) val = updateA memory (pt, IntVal val)
    --def memory int val = (P int, insertA memory (int, IntVal val))
    get (P pt) = StateOp(\mem -> case lookupA mem pt of IntVal val -> (val, mem))
    set (P pt) val = StateOp(\mem -> ((), updateA mem (pt, IntVal val)))
    def pt val = StateOp(\mem -> ((P pt), insertA mem (pt, IntVal val)))

instance Mutable Bool where
    -- TODO: error checking

    --get memory (P pt) = case lookupA memory pt of BoolVal x -> x
    --set memory (P pt) val = updateA memory (pt, BoolVal val)
    --def memory int val = (P int, insertA memory (int, BoolVal val))
    get (P pt) = StateOp(\mem -> case lookupA mem pt of BoolVal val -> (val, mem))
    set (P pt) val = StateOp(\mem -> ((), updateA mem (pt, BoolVal val)))
    def pt val = StateOp(\mem -> ((P pt), insertA mem (pt, BoolVal val)))


alloc :: Mutable a => a -> StateOp (Pointer a)
alloc = getNewAddress >~> \x -> def x a

getNewAddress :: Mutable a => a -> StateOp (Pointer a)
getNewAddress val = StateOp(\mem -> ( (P (1 + fst (head (sortMem mem)))), mem))

free :: Mutable a => Pointer a -> StateOp ()
free = undefined
