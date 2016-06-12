module Object where

import Story
import Text.Printf
import Type
import Utility
import Zstring

{- The object table is laid out as follows:

* The base of the object table is in the header.
* The object table begins with a block of 31 or 63 default property values.
* Following the default property values is the object tree.
* Each entry in the tree is of the same size, and is laid out as follows:
  * 32 or 48 bits of attribute flags
  * the parent, sibling and child object numbers
  * the address of an additional table of variable-sized properties.
* object numbers are one-based, so zero is used as the invalid object.
-}

defaultPropertyTableEntrySize :: Int
defaultPropertyTableEntrySize = 2

defaultPropertyTableSize :: Story.T -> Int
defaultPropertyTableSize story =
  if Story.v3_OrLower (Story.version story) then 31 else 63


treeBase :: Story.T -> ObjectTreeBase
treeBase story =
  let (ObjectBase base) = Story.objectTableBase story in
  let tableSize = defaultPropertyTableSize story in
  ObjectTreeBase (base + defaultPropertyTableEntrySize * tableSize)

entrySize :: Story.T -> Int
entrySize story =
  if Story.v3_OrLower (Story.version story) then 9 else 14

address :: Story.T -> ObjectNumber -> ObjectAddress
address story (Object obj) =
  let (ObjectTreeBase treeBase') = treeBase story in
  let entrySize' = entrySize story in
    ObjectAddress (treeBase' + (obj - 1) * entrySize')

parent :: Story.T -> ObjectNumber -> ObjectNumber
parent story obj =
  let (ObjectAddress addr) = address story obj in
  if Story.v3_OrLower (Story.version story) then
    Object (Story.readByte story (ByteAddress (addr + 4)))
  else
    Object (Story.readWord story (WordAddress (addr + 6)))

sibling :: Story.T -> ObjectNumber -> ObjectNumber
sibling story obj =
  let (ObjectAddress addr) = address story obj in
  if Story.v3_OrLower (Story.version story) then
    Object (Story.readByte story (ByteAddress (addr + 5)))
  else
    Object (Story.readWord story (WordAddress (addr + 8)))

child :: Story.T -> ObjectNumber -> ObjectNumber
child story obj =
  let (ObjectAddress addr) = address story obj in
  if Story.v3_OrLower (Story.version story) then
    Object (Story.readByte story (ByteAddress(addr + 6)))
  else
    Object (Story.readWord story (WordAddress(addr + 10)))

{- The last two bytes in an object description are a pointer to a
block that contains additional properties. -}
propertyHeaderAddress :: Story.T -> ObjectNumber -> PropertyHeaderAddress
propertyHeaderAddress story obj =
  let objectPropertyOffset = if Story.v3_OrLower (Story.version story) then 7 else 12 in
      let (ObjectAddress addr) = address story obj in
      PropertyHeader (Story.readWord story (WordAddress (addr + objectPropertyOffset)))

-- (* Oddly enough, the Z machine does not ever say how big the object table is.
--    Assume that the address of the first property block in the first object is
--    the bottom of the object tree table. *)
count :: Story.T -> Int
count story =
  let (ObjectTreeBase tableStart) = treeBase story in
  let (PropertyHeader tableEnd) = propertyHeaderAddress story (Object 1) in
  let entrySize' = entrySize story in
  (tableEnd - tableStart) `div` entrySize'

{- The property entry begins with a length-prefixed zstring -}
name :: Story.T -> ObjectNumber -> String
name story n =
  let (PropertyHeader addr) = propertyHeaderAddress story n in
  let len = Story.readByte story (ByteAddress addr) in
  if len == 0 then "<unnamed>"
  else Zstring.read story (Zstring (addr + 1))

displayObjectTable :: Story.T -> String
displayObjectTable story =
  let cnt = count story in
  let toString i =
        let current = Object i in
        let (Object parent') = parent story current in
        let (Object sibling') = sibling story current in
        let (Object child') = child story current in
        let name' = name story current in
          printf "%02x: %02x %02x %02x %s\n" i parent' sibling' child' name' in
  accumulateStringsLoop toString 1 (cnt + 1)