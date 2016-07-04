{-# LANGUAGE BinaryLiterals #-}
module UtilitySpec (
    hunitTests,
    -- qcTests
  ) where

import Data.Bits
import Utility

import Test.HUnit

-- import Test.Framework (defaultMain, testGroup, Test)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
-- import Test.QuickCheck

hunitTests :: IO Counts
hunitTests = runTestTT (
              TestList 
                [
                 TestLabel "testAccumulateStringsWithIntArray" testAccumulateStringsWithIntArray,
                 TestLabel "testAccumulateStringsEmptyInput" testAccumulateStringsEmptyInput,
                 TestLabel "testFetchBitsGettingThe4HighBits" testFetchBitsGettingThe4HighBits,
                 TestLabel "testShiftL" testShiftL,
                 TestLabel "testComplement" testComplement,
                 TestLabel "testShiftR" testShiftR,
                 TestLabel "testMask" testMask
                ]
              )

testAccumulateStringsEmptyInput :: Test
testAccumulateStringsEmptyInput =
  TestCase (assertEqual "for (accumulateStrings show [])," "" (accumulateStrings showEmpty []))
  where
    showEmpty :: a -> String
    showEmpty _ = ""

testAccumulateStringsWithIntArray :: Test
testAccumulateStringsWithIntArray =
  TestCase (assertEqual "for (accumulateStrings show [1, 2, 3])," ['1', '2', '3'] (accumulateStrings show [1 :: Integer, 2, 3]))

testFetchBitsGettingThe4HighBits :: Test
testFetchBitsGettingThe4HighBits =
  TestCase (assertEqual "for (fetchBits bit15 size4 0xBEEF)," (0b0000000000001011 :: Int) (fetchBits bit15 size4 0b1011111011101111))

testShiftL :: Test
testShiftL =
  TestCase (assertEqual "for ((-0b0000000000000001) `shiftL` 4 :: Int)," (-0b0000000000010000 :: Int) ((-0b0000000000000001) `shiftL` 4 :: Int))

testComplement :: Test
testComplement =
  TestCase (assertEqual "for (complement (-0b0000000000010000))," (0b0000000000001111 :: Int) (complement (-0b0000000000010000)))

testShiftR :: Test
testShiftR =
  TestCase (assertEqual "for (0b1011111011101111 `shiftR` 12)," (0b0000000000001011 :: Int) (0b1011111011101111 `shiftR` 12))

testMask :: Test
testMask =
  TestCase (assertEqual "for (0b0000000000001011 .&. 0b0000000000001111)," (0b0000000000001011 :: Int) (0b0000000000001011 .&. 0b0000000000001111))

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