module Utility where

import Data.Bits
import Type

bit0 = BitNumber 0
bit1 = BitNumber 1
bit2 = BitNumber 2
bit3 = BitNumber 3
bit4 = BitNumber 4
bit5 = BitNumber 5
bit6 = BitNumber 6
bit7 = BitNumber 7
bit8 = BitNumber 8
bit9 = BitNumber 9
bit10 = BitNumber 10
bit11 = BitNumber 11
bit12 = BitNumber 12
bit13 = BitNumber 13
bit14 = BitNumber 14
bit15 = BitNumber 15

size1 = BitSize 1
size2 = BitSize 2
size3 = BitSize 3
size4 = BitSize 4
size5 = BitSize 5
size6 = BitSize 6
size7 = BitSize 7

-- These helper functions can take advanage of similar ones in Data.Bits.
fetchBit (BitNumber n) word =
  testBit word n

clear_bit (BitNumber n) word =
  clearBit word n

set_bit (BitNumber n) word =
  setBit word n

-- set_bit and clear_bit require BitNumber, so no need to re-specify it here.
setBitTo n word value =
  if value then set_bit n word
  else clear_bit n word

fetchBits (BitNumber high) (BitSize length) word =
  let mask = complement ((-1) `shiftL` length) in
  (word `shiftR` (high - length + 1)) .&. mask