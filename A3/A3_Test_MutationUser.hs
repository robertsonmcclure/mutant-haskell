import Test.HUnit
-- NOTE: Make sure your file exports all the required names,
-- even if you didn't finish everything!
import Mutation (
    Memory, Pointer(..), Value(..),
    Mutable, get, StateOp(..))
import MutationUser (
    pointerTest,
    swap,
    swapCycle)
import Data.List (sortBy, intersect, nub)

-- helper to run a StateOp
run :: StateOp a -> Memory -> (a, Memory)
run (StateOp f) mem = f mem

-- helpers to sort memory
compareFirst :: Ord a => (a, b) -> (a, b) -> Ordering
compareFirst (x, _) (y, _) = compare x y

sortMem :: (a, Memory) -> Memory
sortMem (_, mem) = sortBy compareFirst mem

sortMem2 :: Memory -> Memory
sortMem2 mem = sortBy compareFirst mem

-- instance necessary to compare "values"
instance (Eq Value) where
    IntVal n == IntVal m = n == m
    BoolVal x == BoolVal y = x == y
    _ == _ = False


-- Test data
testMem :: Memory
testMem = [(1, IntVal 10), (2, IntVal 30), (3, IntVal 300), (4, BoolVal False)]

p1 :: Pointer Integer
p1 = P 1

p2 :: Pointer Integer
p2 = P 2

p3 :: Pointer Integer
p3 = P 3

p4 :: Pointer Bool
p4 = P 4


-- The tests
swapTests :: Test
swapTests = TestList [
    [(1, IntVal 30), (2, IntVal 10), (3, IntVal 300), (4, BoolVal False)] ~=?
        sortMem (run (swap p1 p2) testMem),
    [(1, IntVal 30), (2, IntVal 300), (3, IntVal 10), (4, BoolVal False)] ~=?
        sortMem (run (swapCycle [p1, p2, p3]) testMem),
    testMem ~=?
        sortMem (run (swapCycle [p1]) testMem)
    ]

pointerTestTests :: Test
pointerTestTests =
    let ((p1, p2), newMem) = run (pointerTest 3) testMem
    in
        TestList [
            6 ~=? fst (run (get p1) newMem),
            True ~=? fst (run (get p2) newMem),
            testMem ++ [(100, IntVal 6), (500, BoolVal True)] ~=?
                sortMem2 newMem
            ]


main :: IO ()
main = do
    printCount "swapTests" swapTests
    printCount "pointerTestTests" pointerTestTests
    -- Uncomment these to see more detailed test results.
    -- (Format's a little ugly, though.)
    --runTestTT swapTests
    --runTestTT pointerTestTests
    putStrLn "Tests Done!"


-- Test runner helpers. You are not responsible for this code.
justGetCount :: Test -> IO Counts
justGetCount tests = fmap fst (runTestText (PutText (\_ _ () -> return ())  ()) tests)

printCount :: String -> Test -> IO ()
printCount name tests = do
        count <- justGetCount tests
        putStrLn (name ++ ": " ++ show count)
