{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
    removeA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA ((a,b):xs) v = if (a == v) then b else lookupA xs v

-- | Returns a new association list which is the old one, except with
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA ((a,b):xs) (key, val) = if (a == key) then (a,b):xs else (a,b):(insertA xs (key, val))
insertA [] (key, val) = (key, val):[]

-- | Returns a new association list which is the old one, except with
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA ((a,b):xs) (key, val) = if (a == key) then (a, val):xs else (a,b):(updateA xs (key, val))
updateA [] (key, val) = []

removeA :: Eq a => AList a b -> a -> AList a b
removeA ((a,b):xs) index = if (a == index) then xs else (a,b):(removeA xs index)
removeA [] index = []
