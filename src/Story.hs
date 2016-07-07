module Story where

import           Data.Bits
import           ImmutableBytes
import           Text.Printf
import           Type
import           Utility

headerSize :: Int
headerSize = 64

staticMemoryBaseOffset :: WordAddress
staticMemoryBaseOffset = WordAddress 14

versionOffset :: ByteAddress
versionOffset = ByteAddress 0

data T = T { dynamicMemory :: ImmutableBytes.T
           , staticMemory  :: String
           }
make :: String -> String -> Story.T
make dynamic static =
    Story.T { dynamicMemory = ImmutableBytes.make dynamic
            , staticMemory = static
            }

abbreviationsTableBase :: Story.T -> AbbrevTableBase
abbreviationsTableBase story =
  let abbreviationsTableBaseOffset = WordAddress 24 in
  AbbreviationTableBase (readWord story abbreviationsTableBaseOffset)

dictionaryBase :: Story.T -> DictionaryBase
dictionaryBase story =
  let dictionaryBaseOffset = WordAddress 8 in
  DictionaryBase (readWord story dictionaryBaseOffset)

load :: FilePath -> IO Story.T
load filename = do
  file <- getFile filename
  let len = length file in
    if len < headerSize
      then error (printf "%s is not a valid story file" filename)
      else let high = dereferenceString (addressOfHighByte staticMemoryBaseOffset) file
               low = dereferenceString (addressOfLowByte staticMemoryBaseOffset) file
               dynamicLength = high * 256 + low in
            if dynamicLength > len
              then error (printf "%s is not a valid story file" filename)
              else let dynamic = take dynamicLength file in
                let static = drop dynamicLength file in
                  return (Story.make dynamic static)

objectTableBase :: Story.T -> ObjectBase
objectTableBase story =
  let objectTableBaseOffset = WordAddress 10 in
  ObjectBase (readWord story objectTableBaseOffset)

readByte :: Story.T -> ByteAddress -> Int
readByte story address =
  let dynamicSize = size (dynamicMemory story) in
  if isInRange address dynamicSize then
    ImmutableBytes.readByte (dynamicMemory story) address
  else
    let staticAddr = decByteAddrBy address dynamicSize in
    dereferenceString staticAddr (staticMemory story)

readWord :: Story.T -> WordAddress -> Int
readWord story address =
  let high = Story.readByte story (addressOfHighByte address) in
  let low = Story.readByte story (addressOfLowByte address) in
    256 * high + low

writeByte :: Story.T -> ByteAddress -> Int -> Story.T
writeByte story address value =
  let newDynamicMemory = ImmutableBytes.writeByte (dynamicMemory story) address value in
    story { dynamicMemory = newDynamicMemory }

writeWord :: Story.T -> WordAddress -> Int -> Story.T
writeWord story address value =
  let high = shiftR value 8 .&. 0xFF in
  let low = value .&. 0xFF in
  let localStory = Story.writeByte story (addressOfHighByte address) high in
    Story.writeByte localStory (addressOfLowByte address) low

v3_OrLower :: Version -> Bool
v3_OrLower v | v `elem` [V1, V2, V3] = True
             | v `elem` [V4, V5, V6, V7, V8] = False
             | otherwise = error "unknown version"

v4_OrLower :: Version -> Bool
v4_OrLower v | v `elem` [V1, V2, V3, V4] = True
             | v `elem` [V5, V6, V7, V8] = False
             | otherwise = error "unknown version"

v4_OrHigher :: Version -> Bool
v4_OrHigher v = not (v3_OrLower v)

v5_OrHigher :: Version -> Bool
v5_OrHigher v | v `elem` [V5, V6, V7, V8] = True
              | v `elem` [V1, V2, V3, V4] = False
              | otherwise = error "unknown version"

version :: Story.T -> Version
version story =
  case Story.readByte story versionOffset of
    1 -> V1
    2 -> V2
    3 -> V3
    4 -> V4
    5 -> V5
    6 -> V6
    7 -> V7
    8 -> V8
    _ -> error "unknown version"
