module Utility where

import Data.Bits
import Data.ByteString.Lazy as LBS
import Data.Char
import Language.Haskell.TH.Ppr
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

addressOfHighByte (WordAddress address) =
  ByteAddress address

addressOfLowByte (WordAddress address) =
  ByteAddress (address + 1)

byteOfInt x =
  x .&. 0xff

decByteAddrBy address offset =
  incByteAddrBy address (negate offset)

dereferenceString address bytes =
  if isOutOfRange address (Prelude.length bytes)
    then error "address out of range"
    else let (ByteAddress addr) = address in
      ord (bytes !! addr)

-- fetchBit (BitNumber n) word =
--   testBit word n

fetchBits (BitNumber high) (BitSize length) word =
  let mask = complement ((-1) `shiftL` length) in
    (word `shiftR` (high - length + 1)) .&. mask

getFile filePath = do
    contents <- LBS.readFile filePath
    return $ bytesToString (unpack contents)

incByteAddrBy (ByteAddress address) offset =
  ByteAddress (address + offset)

incWordAddrBy (WordAddress address) offset =
  WordAddress (address + offset * wordSize)

incWordAddr address =
  incWordAddrBy address 1

isInRange (ByteAddress address) size =
    0 <= address && address < size

isOutOfRange address size =
    not (isInRange address size)

setBitTo (BitNumber n) word value =
  if value
    then setBit n word
    else clearBit n word

wordSize = 2