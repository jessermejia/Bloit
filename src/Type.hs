module Type where

newtype AbbreviationNumber = Abbreviation Int deriving (Show)
newtype AbbrevTableBase = AbbreviationTableBase Int deriving (Show)
data Bit = Zero | One deriving (Eq,Show)
newtype BitNumber = BitNumber Int deriving (Show)
newtype BitSize = BitSize Int deriving (Show)
newtype ByteAddress = ByteAddress Int deriving (Show)
newtype WordAddress = WordAddress Int deriving (Show)
newtype WordZstringAddress = WordZstring Int deriving (Show)
newtype Zchar = Zchar Int deriving (Show)
newtype ZstringAddress = Zstring Int deriving (Show)