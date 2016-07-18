{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Type where

import           Text.Printf

newtype AbbreviationNumber = Abbreviation Int deriving (Show)
newtype AbbrevTableBase = AbbreviationTableBase Int deriving (Show)

newtype BitNumber = BitNumber Int deriving (Show)
newtype BitSize = BitSize Int deriving (Show)
newtype ByteAddress = ByteAddress Int deriving (Show)

newtype DictionaryBase = DictionaryBase Int deriving (Show)
newtype DictionaryTableBase = DictionaryTableBase Int deriving (Show)
newtype DictionaryAddress = DictionaryAddress Int deriving (Show)
newtype DictionaryNumber = Dictionary Int deriving (Show)

newtype InstructionAddress = Instruction Int deriving (Eq,Ord,PrintfArg,Show)
newtype LocalVariable = Local Int deriving (Show)
newtype GlobalVariable = Global Int deriving (Show)

newtype ObjectBase = ObjectBase Int deriving (Show)
newtype ObjectTreeBase = ObjectTreeBase Int deriving (Show)
newtype ObjectNumber = Object Int deriving (Show,Eq)
newtype ObjectAddress = ObjectAddress Int deriving (Show)
newtype PackedRoutineAddress = PackedRoutine Int
newtype PackedZstringAddress = PackedZstring Int
newtype PropertyDefaultsTable = PropertyDefaultsTable Int
newtype PropertyHeaderAddress = PropertyHeader Int
newtype RoutineAddress = Routine Int
newtype WordAddress = WordAddress Int deriving (Show)
newtype WordZstringAddress = WordZstring Int deriving (Show)

newtype Zchar = Zchar Int deriving (Show)
newtype ZstringAddress = Zstring Int deriving (Show)

data BranchAddress = ReturnTrue
                   | ReturnFalse
                   | BranchAddress InstructionAddress

data Bytecode = OP2_1   | OP2_2   | OP2_3   | OP2_4   | OP2_5   | OP2_6   | OP2_7
    | OP2_8   | OP2_9   | OP2_10  | OP2_11  | OP2_12  | OP2_13  | OP2_14  | OP2_15
    | OP2_16  | OP2_17  | OP2_18  | OP2_19  | OP2_20  | OP2_21  | OP2_22  | OP2_23
    | OP2_24  | OP2_25  | OP2_26  | OP2_27  | OP2_28
    | OP1_128 | OP1_129 | OP1_130 | OP1_131 | OP1_132 | OP1_133 | OP1_134 | OP1_135
    | OP1_136 | OP1_137 | OP1_138 | OP1_139 | OP1_140 | OP1_141 | OP1_142 | OP1_143
    | OP0_176 | OP0_177 | OP0_178 | OP0_179 | OP0_180 | OP0_181 | OP0_182 | OP0_183
    | OP0_184 | OP0_185 | OP0_186 | OP0_187 | OP0_188 | OP0_189 | OP0_190 | OP0_191
    | VAR_224 | VAR_225 | VAR_226 | VAR_227 | VAR_228 | VAR_229 | VAR_230 | VAR_231
    | VAR_232 | VAR_233 | VAR_234 | VAR_235 | VAR_236 | VAR_237 | VAR_238 | VAR_239
    | VAR_240 | VAR_241 | VAR_242 | VAR_243 | VAR_244 | VAR_245 | VAR_246 | VAR_247
    | VAR_248 | VAR_249 | VAR_250 | VAR_251 | VAR_252 | VAR_253 | VAR_254 | VAR_255
    | EXT_0   | EXT_1   | EXT_2   | EXT_3   | EXT_4   | EXT_5   | EXT_6   | EXT_7
    | EXT_8   | EXT_9   | EXT_10  | EXT_11  | EXT_12  | EXT_13  | EXT_14
    | EXT_16  | EXT_17  | EXT_18  | EXT_19  | EXT_20  | EXT_21  | EXT_22  | EXT_23
    | EXT_24  | EXT_25  | EXT_26  | EXT_27  | EXT_28  | EXT_29
    | ILLEGAL deriving (Eq)

data Operand = Large Int
             | Small Int
             | Variable VariableLocation

data VariableLocation = Stack
                      | LocalVariable LocalVariable
                      | GlobalVariable GlobalVariable

data Version = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 deriving (Enum,Eq)
