import           Test.HUnit
import           UtilitySpec

main =
  -- UtilitySpec.qcTests
  UtilitySpec.hunitTests


{- example testframework/quickcheck tests -}
-- import Test.Framework (defaultMain, testGroup)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Test.QuickCheck

-- runTests :: IO ()
-- runTests = defaultMain tests

-- tests = [
--         testGroup "Sorting Group 1" [
--                 testProperty "prop1" prop1,
--                 testProperty "prop2" prop2
--            ]
--       ]

-- prop1 b = b == False
--   where types = (b :: Bool)

-- prop2 i = i == 42
--   where types = (i :: Int)

{- example hunit tests -}
-- module Example where
-- import Test.HUnit


-- foo :: Int -> (Int, Int)
-- foo x = (1, x)

-- partA :: Int -> IO (Int, Int)
-- partA v = return (v+2, v+3)

-- partB :: Int -> IO Bool
-- partB v = return (v > 5)

-- test1 :: Test
-- test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 2))

-- test2 :: Test
-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

-- tests :: Test
-- tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

-- tests' :: Test
-- tests' = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? foo 2,
--                 "test2" ~: do (x, y) <- partA 3
--                               assertEqual "for the first result of partA," 5 x
--                               partB y @? "(partB " ++ show y ++ ") failed" ]

-- main :: IO Counts
-- main = do _ <- runTestTT tests
--           runTestTT tests'
