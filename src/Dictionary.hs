module Dictionary where

import           Story
import           Text.Printf
import           Type
import           Utility
import           Zstring

{- The table is laid out as follows. First there is a header:
byte giving the number of word separators
the word separators, one byte each
byte giving the number of bytes in each dictionary entry
word giving the number of table entries which follow
Each entry is either 4 (in V1-3) or 6 (otherwise) bytes of zstring data,
followed by enough bytes to make up the size of the dictionary entry. -}

display :: Story.T -> String
display story =
  let count = entryCount story
      toString i = printf "%s " (entry story (Dictionary i)) in
  accumulateStringsLoop toString 0 count

entry :: Story.T -> DictionaryNumber -> String
entry story dictionaryNumber =
  let (DictionaryAddress addr) = entryAddress story dictionaryNumber in
  Zstring.read story (Zstring addr)

entryAddress :: Story.T -> DictionaryNumber -> DictionaryAddress
entryAddress story (Dictionary dictionaryNumber) =
  let (DictionaryTableBase base) = tableBase story
      len = entryLength story in
  DictionaryAddress (base + dictionaryNumber * len)

entryBase :: Story.T -> ByteAddress
entryBase story =
  let dictBase = Story.dictionaryBase story
      wsCount = wordSeparatorsCount story
      wsBase = wordSeparatorsBase dictBase in
  incByteAddrBy wsBase (wsCount + 1)

entryCount :: Story.T -> Int
entryCount story =
  let (ByteAddress addr) = incByteAddr (entryBase story) in
  Story.readWord story (WordAddress addr)

entryLength :: Story.T -> Int
entryLength story =
  Story.readByte story (entryBase story)

{- This is the address of the actual dictionary entries, past the initial
header with the word separators. -}
tableBase :: Story.T -> DictionaryTableBase
tableBase story =
  let (ByteAddress addr) = incByteAddrBy (entryBase story) 3 in
  DictionaryTableBase addr

wordSeparatorsBase :: DictionaryBase -> ByteAddress
wordSeparatorsBase (DictionaryBase base) =
  ByteAddress base

wordSeparatorsCount :: Story.T -> Int
wordSeparatorsCount story =
  let dictBase = Story.dictionaryBase story
      wsBase = wordSeparatorsBase dictBase in
  Story.readByte story wsBase
