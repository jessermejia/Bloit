module Utility where

import Data.Bits
import Data.ByteString.Lazy as LBS
import Data.Char
import Language.Haskell.TH.Ppr
import Type

bit0 :: BitNumber
bit1 :: BitNumber
bit2 :: BitNumber
bit3 :: BitNumber
bit4 :: BitNumber
bit5 :: BitNumber
bit6 :: BitNumber
bit7 :: BitNumber
bit8 :: BitNumber
bit9 :: BitNumber
bit10 :: BitNumber
bit11 :: BitNumber
bit12 :: BitNumber
bit13 :: BitNumber
bit14 :: BitNumber
bit15 :: BitNumber

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

size1 :: BitSize
size2 :: BitSize
size3 :: BitSize
size4 :: BitSize
size5 :: BitSize
size6 :: BitSize
size7 :: BitSize

size1 = BitSize 1
size2 = BitSize 2
size3 = BitSize 3
size4 = BitSize 4
size5 = BitSize 5
size6 = BitSize 6
size7 = BitSize 7

wordSize :: Int
wordSize = 2

accumulateStringsLoop :: (Int -> String) -> Int -> Int -> String
accumulateStringsLoop toString start tooFar =
  aux "" start
  where
    aux :: String -> Int -> String
    aux acc i =
      if i >= tooFar
        then acc
        else aux (acc ++ toString i) (i + 1)
  

addressOfHighByte :: WordAddress -> ByteAddress
addressOfHighByte (WordAddress address) =
  ByteAddress address

addressOfLowByte :: WordAddress -> ByteAddress
addressOfLowByte (WordAddress address) =
  ByteAddress (address + 1)

byteOfInt :: (Num a, Bits a) => a -> a
byteOfInt x =
  x .&. 0xff

decByteAddrBy :: ByteAddress -> Int -> ByteAddress
decByteAddrBy address offset =
  incByteAddrBy address (negate offset)

dereferenceString :: ByteAddress -> String -> Int
dereferenceString address bytes =
  if isOutOfRange address (Prelude.length bytes)
    then error "address out of range"
    else let (ByteAddress addr) = address in
      ord (bytes !! addr)

fetchBit :: Bits a => BitNumber -> a -> Bool
fetchBit (BitNumber n) word =
  testBit word n

fetchBits :: (Num a, Bits a) => BitNumber -> BitSize -> a -> a
fetchBits (BitNumber high) (BitSize len) word =
  let mask = complement ((-1) `shiftL` len) in
    (word `shiftR` (high - len + 1)) .&. mask

getFile :: FilePath -> IO String
getFile filePath = do
    contents <- LBS.readFile filePath
    return $ bytesToString (unpack contents)

incByteAddrBy :: ByteAddress -> Int -> ByteAddress
incByteAddrBy (ByteAddress address) offset =
  ByteAddress (address + offset)

incByteAddr :: ByteAddress -> ByteAddress
incByteAddr address = 
    incByteAddrBy address 1

incWordAddrBy :: WordAddress -> Int -> WordAddress
incWordAddrBy (WordAddress address) offset =
  WordAddress (address + offset * wordSize)

incWordAddr :: WordAddress -> WordAddress
incWordAddr address =
  incWordAddrBy address 1

isInRange :: ByteAddress -> Int -> Bool
isInRange (ByteAddress address) size =
    0 <= address && address < size

isOutOfRange :: ByteAddress -> Int -> Bool
isOutOfRange address size =
    not (isInRange address size)

setBitTo :: BitNumber -> Int -> Bool -> Int
setBitTo (BitNumber n) word value =
  if value
    then setBit n word
    else clearBit n word