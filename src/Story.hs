module Story where

import Data.Bits
import ImmutableBytes
import Text.Printf
import Type
import Utility

data T = T { dynamicMemory :: ImmutableBytes.T
           , staticMemory :: String
           }

make dynamic static =
    Story.T { dynamicMemory = ImmutableBytes.make dynamic
            , staticMemory = static
            }

headerSize = 64
staticMemoryBaseOffset = WordAddress 14
load filename = do
  file <- getFile filename
  let len = length file in
    if len < headerSize
      then error (printf "%s is not a valid story file" filename)
      else let high = dereferenceString (addressOfHighByte staticMemoryBaseOffset) file in
          let low = dereferenceString (addressOfLowByte staticMemoryBaseOffset) file in
          let dynamicLength = high * 256 + low in
            if dynamicLength > len
              then error (printf "%s is not a valid story file" filename)
              else let dynamic = take dynamicLength file in
              --  else let dynamic = String.sub file 0 dynamicLength in
                let static = take (len - dynamicLength) file in
                --  let static = String.sub file dynamicLength (len - dynamicLength) in
                  return (Story.make dynamic static)

readByte story address =
  let dynamicSize = size (dynamicMemory story) in
  if isInRange address dynamicSize then
    ImmutableBytes.readByte (dynamicMemory story) address
  else
    let staticAddr = decByteAddrBy address dynamicSize in
    dereferenceString staticAddr (staticMemory story)

readWord story address =
  let high = Story.readByte story (addressOfHighByte address) in
  let low = Story.readByte story (addressOfLowByte address) in
    256 * high + low

-- Story.writeByte :: Story.T -> ByteAddress -> Int -> Story.T
writeByte story address value =
  let newDynamicMemory = ImmutableBytes.writeByte (dynamicMemory story) address value in
    story { dynamicMemory = newDynamicMemory }

writeWord story address value =
  let high = shiftR value 8 .&. 0xFF in
  let low = value .&. 0xFF in
  let story = Story.writeByte story (addressOfHighByte address) high in
    Story.writeByte story (addressOfLowByte address) low