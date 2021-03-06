module Utility where

import           Data.Bits
import           Data.ByteString.Lazy    as LBS hiding (elem)
import           Data.Char
import           Language.Haskell.TH.Ppr
import           Type

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

accumulateStrings :: Foldable t1 => (t -> String) -> t1 t -> String
accumulateStrings toString items =
  Prelude.foldl folder "" items
    where
      folder text item = text ++ toString item

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

byteAddrToWordAddr :: ByteAddress -> WordAddress
byteAddrToWordAddr (ByteAddress address) =
  WordAddress address

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

signedWord :: Int -> Int
signedWord word =
  let canonical = unsignedWord word in
  if canonical > 32767 then canonical - 65536 else canonical

unsignedWord :: Int -> Int
unsignedWord word =
  ((word `mod` 65536) + 65536) `mod` 65536

{- Helper method that takes an item and a function that produces related items.
 The result is the transitive closure of the relation. -}

{- TODO: This is not very efficient because of the call to List.mem in there.
   TODO: A solution involving an immutable set would be more performant for
   TODO: large closures. -}


transitiveClosureMany :: Eq a => [a] -> (a -> [a]) -> [a]
transitiveClosureMany items relation =
  aux [] items
  where
    merge related set stack =
      case related of
        []          -> (set, stack)
        head' : tail' ->
          if head' `elem` set
            then merge tail' set stack
            else merge tail' (head' : set) (head' : stack)
    aux set stack =
      case stack of
        []          -> set
        head' : tail' ->
          let (newSet, newStack) = merge (relation head') set tail' in
            aux newSet newStack

transitiveClosure :: Eq a => a -> (a -> [a]) -> [a]
transitiveClosure item relation =
  transitiveClosureMany [item] relation

reflexiveClosureMany :: Eq a => [a] -> (a -> [a]) -> [a]
reflexiveClosureMany items relation =
  let t = transitiveClosureMany items relation in
  Prelude.foldl (\s i -> if i `elem` s then s else i : s) t items

reflexiveClosure :: Eq a => a -> (a -> [a]) -> [a]
reflexiveClosure item relation =
  reflexiveClosureMany [item] relation
