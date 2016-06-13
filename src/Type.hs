module Type where

data Version = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 deriving (Enum,Eq)

newtype AbbreviationNumber = Abbreviation Int deriving (Show)
newtype AbbrevTableBase = AbbreviationTableBase Int deriving (Show)

newtype BitNumber = BitNumber Int deriving (Show)
newtype BitSize = BitSize Int deriving (Show)
newtype ByteAddress = ByteAddress Int deriving (Show)

newtype DictionaryBase = DictionaryBase Int deriving (Show)
newtype DictionaryTableBase = DictionaryTableBase Int deriving (Show)
newtype DictionaryAddress = DictionaryAddress Int deriving (Show)
newtype DictionaryNumber = Dictionary Int deriving (Show)

newtype ObjectBase = ObjectBase Int deriving (Show)
newtype ObjectTreeBase = ObjectTreeBase Int deriving (Show)
newtype ObjectNumber = Object Int deriving (Show,Eq)
newtype ObjectAddress = ObjectAddress Int deriving (Show)

newtype PropertyDefaultsTable = PropertyDefaultsTable Int
newtype PropertyHeaderAddress = PropertyHeader Int

newtype WordAddress = WordAddress Int deriving (Show)
newtype WordZstringAddress = WordZstring Int deriving (Show)

newtype Zchar = Zchar Int deriving (Show)
newtype ZstringAddress = Zstring Int deriving (Show)

