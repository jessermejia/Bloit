module ImmutableBytes where

import qualified Data.IntMap.Lazy as IntMap
import Data.Char
import Data.Maybe
import Type
import Utility

data T = T { originalBytes :: String
           , edits :: IntMap.IntMap Char
           }

make bytes = T { originalBytes = bytes
               , edits = IntMap.empty
               }

size bytes =
  length (originalBytes bytes)

readByte :: T -> ByteAddress -> Int
readByte bytes address =
  if isOutOfRange address (size bytes)
    then error "address is out of range"
    else let (ByteAddress addr) = address in
        let c = if IntMap.member addr (edits bytes)
                  then fromJust (IntMap.lookup addr (edits bytes))
                  else originalBytes bytes !! addr in
        ord c

writeByte bytes address value =
  if isOutOfRange address (size bytes)
    then error "address is out of range"
    else let (ByteAddress addr) = address in
      let b = chr (byteOfInt value) in
       bytes { edits = IntMap.insert addr b (edits bytes) }

original bytes = bytes { edits = IntMap.empty }