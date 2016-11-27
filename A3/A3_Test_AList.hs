import Test.HUnit
import AList (
    AList,
    lookupA,
    insertA,
    updateA)
import Data.List (sort)


aListTests :: Test
aListTests = TestList [
    "hi" ~=? lookupA [(1, "hi"), (2, "bye")] 1,
    "bye" ~=? lookupA [(1, "hi"), (2, "bye")] 2,
    [(1, "hi"), (2, "bye"), (10, "test")] ~=?
        sort (insertA [(1, "hi"), (2, "bye")] (10, "test")),
    [(1, "hi"), (2, "bye")] ~=?
        insertA [(1, "hi"), (2, "bye")] (2, "test"),
    [(1, "hi"), (2, "TEST")] ~=?
        sort (updateA [(1, "hi"), (2, "bye")] (2, "TEST")),
    [(1, "hi"), (2, "bye")] ~=?
        updateA [(1, "hi"), (2, "bye")] (10, "t")
    ]


main :: IO ()
main = do
    printCount "AList" aListTests
    -- Uncomment this to see more detailed test results.
    -- (Format's a little ugly, though.)
    --runTestTT aListTests
    putStrLn "Tests Done!"


-- Test runner helpers. You are not responsible for this code.
justGetCount :: Test -> IO Counts
justGetCount tests = fmap fst (runTestText (PutText (\_ _ () -> return ())  ()) tests)

printCount :: String -> Test -> IO ()
printCount name tests = do
        count <- justGetCount tests
        putStrLn (name ++ ": " ++ show count)
