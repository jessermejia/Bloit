module Zstring where

import           Data.Char
import           Story
import           Text.Printf
import           Type
import           Utility

data StringState = Alphabet Int
                 | Abbrev AbbreviationNumber
                 | Leading
                 | Trailing Int

abbrev0 :: StringState
abbrev0 = Abbrev (Abbreviation 0)

abbrev32 :: StringState
abbrev32 = Abbrev (Abbreviation 32)

abbrev64 :: StringState
abbrev64 = Abbrev (Abbreviation 64)

alphabet0 :: StringState
alphabet0 = Alphabet 0

alphabet1 :: StringState
alphabet1 = Alphabet 1

alphabet2 :: StringState
alphabet2 = Alphabet 2

abbreviationTableLength :: Int
abbreviationTableLength = 96

{- A "word address" is only used in the abbreviation table, and is always
just half the real address. A "packed address" is used in calls and fetching
strings, and is half the real address in v3 but different for other versions. -}

decodeWordAddress :: WordZstringAddress -> ZstringAddress
decodeWordAddress (WordZstring wordAddress) =
  Zstring (wordAddress * 2)

firstAbbrevAddr :: AbbrevTableBase -> WordAddress
firstAbbrevAddr (AbbreviationTableBase base) =
  WordAddress base

abbreviationZstring :: T -> AbbreviationNumber -> ZstringAddress
abbreviationZstring story (Abbreviation n) =
  if n < 0 || n >= abbreviationTableLength
      then error "bad offset into abbreviation table"
      else let base = firstAbbrevAddr (Story.abbreviationsTableBase story) in
           let abbrAddr = incWordAddrBy base n in
           let wordAddr = WordZstring (Story.readWord story abbrAddr) in
            decodeWordAddress wordAddr

alphabetTable :: [[String]]
alphabetTable =
  [[ " ", "?", "?", "?", "?", "?", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
     "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ],
   [ " ", "?", "?", "?", "?", "?", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
     "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z" ],
   [ " ", "?", "?", "?", "?", "?", "?", "\n", "0", "1", "2", "3", "4", "5", "6", "7",
     "8", "9", ".", ",", "!", "?", "_", "#", "'", "\"", "/", "\\", "-", ":", "(", ")" ]]

displayBytes :: Story.T -> ZstringAddress -> String
displayBytes story (Zstring addr) =
  aux (WordAddress addr) ""
  where
    aux :: WordAddress -> String -> String
    aux current inAcc = let word = Story.readWord story current in
                        let isEnd = fetchBits bit15 size1 word in
                        let zchar1 = fetchBits bit14 size5 word in
                        let zchar2 = fetchBits bit9 size5 word in
                        let zchar3 = fetchBits bit4 size5 word in
                        let s = printf "%02x %02x %02x " zchar1 zchar2 zchar3 in
                        let outAcc = inAcc ++ s in
                          if isEnd == 1
                            then outAcc
                            else aux (incWordAddr current) outAcc

{- gives the length in bytes of the encoded zstring, not the decoded string -}
length :: Story.T -> ZstringAddress -> Int
length story (Zstring address) =
  aux 0 (WordAddress address)
  where
    aux len current =
      if fetchBit bit15 (Story.readWord story current) then len + 2
      else aux (len + 2) (incWordAddr current)

read :: T -> ZstringAddress -> String
read story (Zstring address) =
  aux "" alphabet0 (WordAddress address)
    where
      aux :: String -> StringState -> WordAddress -> String
      aux acc state1 currentAddress =
        let zcharBitSize = size5 in
        let word = Story.readWord story currentAddress in
        let isEnd = fetchBit bit15 word in
        let zchar1 = fetchBits bit14 zcharBitSize word in
        let zchar2 = fetchBits bit9 zcharBitSize word in
        let zchar3 = fetchBits bit4 zcharBitSize word in
        let (text1, state2) = processZchar zchar1 state1 in
        let (text2, state3) = processZchar zchar2 state2 in
        let (text3, stateNext) = processZchar zchar3 state3 in
        let newAcc = acc ++ text1 ++ text2 ++ text3 in
        if isEnd
          then newAcc
          else aux newAcc stateNext (incWordAddr currentAddress)
            where
              processZchar :: Int -> StringState -> (String, StringState)
              processZchar 1 (Alphabet _) = ("", abbrev0)
              processZchar 2 (Alphabet _) = ("", abbrev32)
              processZchar 3 (Alphabet _) = ("", abbrev64)
              processZchar 4 (Alphabet _) = ("", alphabet1)
              processZchar 5 (Alphabet _) = ("", alphabet2)
              processZchar 6 (Alphabet 2) = ("", Leading)
              processZchar zchar (Alphabet a) = ((alphabetTable !! a) !! zchar, alphabet0)
              processZchar zchar (Abbrev (Abbreviation a)) =
                let abbrv = Abbreviation (a + zchar) in
                let addr = abbreviationZstring story abbrv in
                let str = Zstring.read story addr
                in (str, alphabet0)
              processZchar zchar Leading = ("", Trailing zchar)
              processZchar zchar (Trailing high) =
                let s = replicate 1 (chr (high * 32 + zchar))
                in (s, alphabet0)
