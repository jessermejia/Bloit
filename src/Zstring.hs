module Zstring where

import Story
import Text.Printf
import Type
import Utility

abbreviationTableLength = 96

{-| A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. -}

decodeWordAddress (WordZstring wordAddress) =
  Zstring (wordAddress * 2)

firstAbbrevAddr (AbbreviationTableBase base) =
  WordAddress base

abbreviationZstring story (Abbreviation n) =
  if n < 0 || n >= abbreviationTableLength
      then error "bad offset into abbreviation table"
      else let base = firstAbbrevAddr (Story.abbreviationsTableBase story) in
           let abbrAddr = incWordAddrBy base n in
           let wordAddr = WordZstring (Story.readWord story abbrAddr) in
            decodeWordAddress wordAddr

alphabetTable = ["_", "?", "?", "?", "?", "?", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
                 "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

displayBytes :: Story.T -> ZstringAddress -> String
displayBytes story (Zstring addr) =
  aux (WordAddress addr) ""
  where aux current inAcc = let word = Story.readWord story current in
                          let isEnd = fetchBits bit15 size1 word in
                          let zchar1 = fetchBits bit14 size5 word in
                          let zchar2 = fetchBits bit9 size5 word in
                          let zchar3 = fetchBits bit4 size5 word in
                          let s = printf "%02x %s %02x %s %02x %s "
                                         zchar1 (alphabetTable !! zchar1)
                                         zchar2 (alphabetTable !! zchar2)
                                         zchar3 (alphabetTable !! zchar3) in
                          let outAcc = inAcc ++ s in
                              if isEnd == 1
                                then outAcc
                                else aux (incWordAddr current) outAcc