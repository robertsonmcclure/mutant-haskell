import Test.HUnit
-- NOTE: Make sure your file exports all the required names,
-- even if you didn't finish everything!
import Mutation (
    Memory, Pointer(..),
    Mutable, get, set, def,
    StateOp(..), Value(..),
    (>>>), (>~>),
    returnVal, alloc, free)


import Data.List (sortBy, intersect, nub)

-- helper to run a StateOp
run :: StateOp a -> Memory -> (a, Memory)
run (StateOp f) mem = f mem

-- helpers to sort memory
compareFirst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFirst (x, _) (y, _) = compare x y

sortMem :: (a, Memory) -> Memory
sortMem (_, mem) = sortBy compareFirst mem

-- instance necessary to compare "values"
instance (Eq Value) where
    IntVal n == IntVal m = n == m
    BoolVal x == BoolVal y = x == y
    _ == _ = False


-- Test data
testMem :: Memory
testMem = [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)]

p1 :: Pointer Integer
p1 = P 1

p3 :: Pointer Bool
p3 = P 3


-- The tests
mutableTests :: Test
mutableTests = TestList [
    (10, testMem) ~=? run (get p1) testMem,
    (True, testMem) ~=? run (get p3) testMem,
    [(1, IntVal 20), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False)] ~=?
        sortMem (run (set p1 20) testMem),
    [(1, IntVal 10), (2, IntVal 30), (3, BoolVal False), (4, BoolVal False)] ~=?
        sortMem (run (set p3 False) testMem),
    [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False),
     (100, IntVal 42)] ~=? sortMem (run (def 100 (42 :: Integer)) testMem),
    [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False),
     (100, BoolVal False)] ~=? sortMem (run (def 100 False) testMem)
    ]

chainTests :: Test
chainTests =
    let set2 = set p1 0 >>> set p3 False
        defGet = def 100 (42 :: Integer) >~> \p -> get p
    in
        TestList [
            [(1, IntVal 0), (2, IntVal 30),
             (3, BoolVal False), (4, BoolVal False)] ~=?
            sortMem (run set2 testMem),
            42 ~=? fst (run defGet testMem),
            [(1, IntVal 10), (2, IntVal 30), (3, BoolVal True), (4, BoolVal False),
             (100, IntVal 42)] ~=? sortMem (run defGet testMem),
            ("hello, world!", testMem) ~=? run (returnVal "hello, world!") testMem
        ]

safetyTests :: Test
safetyTests =
    let (p, newMem) = run (alloc (42 :: Integer)) testMem
    in
    TestList [
        -- Check there are five unique keys
        5 ~=? length (nub (map fst newMem)),
        -- Check that 42 has indeed been inserted (and no other values changed)
        5 ~=? length (intersect [IntVal 10, IntVal 30, BoolVal True, BoolVal False,
                                 IntVal 42]
                                (map snd newMem)),
        [(1, IntVal 10), (2, IntVal 30), (4, BoolVal False)] ~=?
            sortMem (run (free p3) testMem)
    ]

main :: IO ()
main = do
    printCount "mutableTests" mutableTests
    printCount "chainTests" chainTests
    printCount "safetyTests" safetyTests
    -- Uncomment these to see more detailed test results.
    -- (Format's a little ugly, though.)
    --runTestTT mutableTests
    --runTestTT chainTests
    --runTestTT safetyTests
    putStrLn "Tests Done!"


-- Test runner helpers. You are not responsible for this code.
justGetCount :: Test -> IO Counts
justGetCount tests = fmap fst (runTestText (PutText (\_ _ () -> return ())  ()) tests)

printCount :: String -> Test -> IO ()
printCount name tests = do
        count <- justGetCount tests
        putStrLn (name ++ ": " ++ show count)
