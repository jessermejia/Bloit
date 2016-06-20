module UtilitySpec (
    hunitTests,
    -- qcTests
  ) where
import Utility

import Test.HUnit

-- import Test.Framework (defaultMain, testGroup, Test)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
-- import Test.QuickCheck

hunitTests :: IO Counts
hunitTests = runTestTT (TestList 
              [TestLabel "testAccumulateStringsWithIntArray" testAccumulateStringsWithIntArray,
               TestLabel "testAccumulateStringsEmptyInput" testAccumulateStringsEmptyInput])

testAccumulateStringsEmptyInput :: Test
testAccumulateStringsEmptyInput =
  TestCase (assertEqual "for (accumulateStrings show [])," "" (accumulateStrings showEmpty []))
  where
    showEmpty :: a -> String
    showEmpty _ = ""

testAccumulateStringsWithIntArray :: Test
testAccumulateStringsWithIntArray =
  TestCase (assertEqual "for (accumulateStrings show [1, 2, 3])," ['1', '2', '3'] (accumulateStrings show [1 :: Integer, 2, 3]))

{- trying to use quickcheck... -}

-- qcTests :: IO ()
-- qcTests = defaultMain tests

-- tests :: [Test]
-- tests = [
--         testGroup "Group 1 - accumulateStrings" [
--                 testProperty "accumulateStrings" accumulateStringsProp1
--            ]
--       ]

-- accumulateStringsProp1 :: Blind (Integer -> String) -> [Integer] -> Bool
-- accumulateStringsProp1 (Blind f) [] = accumulateStrings f [] == ""
-- accumulateStringsProp1 (Blind f) xs = head (accumulateStrings f xs) == head (f (head xs))