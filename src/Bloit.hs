module Bloit where

import Data.Bits
import Utility

word = 0xBEEF :: Int

fetchBitsOriginal high length word =
  let mask = complement ((-1) `shiftL` length) in
  (word `shiftR` (high - length + 1)) .&. mask

main = do
  print (shiftR word 12 .&. complement (shiftL (-1) 4))
  print (fetchBitsOriginal 15 4 word)
  print (fetchBits bit15 size4 word)