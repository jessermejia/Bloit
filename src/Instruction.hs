module Instruction where

import Story
import Text.Printf
import Type
import Utility
import Zstring

data T = T {  opcode :: Bytecode
           ,  address :: InstructionAddress
           ,  length :: Int
           ,  operands :: [Operand]
           ,  store :: Maybe VariableLocation
           ,  branch :: Maybe (Bool, BranchAddress)
           ,  text :: Maybe String
           }

data OpcodeForm = LongForm 
                | ShortForm
                | VariableForm 
                | ExtendedForm

data OperandCount = OP0
                  | OP1
                  | OP2
                  | VAR

decodeVariable n | n == 0 = Stack
                 | n <= maximumLocal = LocalVariable (Local n)
                 | otherwise = GlobalVariable (Global n)
                 where
                  maximumLocal = 15



encodeVariable variable =
  case variable of
    Stack -> 0
    LocalVariable (Local n) -> n
    GlobalVariable (Global n) -> n

{- We match Inform's convention of numbering the locals and globals from zero -}
displayVariable variable =
  case variable of
    Stack -> "sp"
    LocalVariable (Local local) -> printf "local%d" (local - 1)
    GlobalVariable (Global global) -> printf "g%02x" (global - 16)

data OperandType = LargeOperand
                 | SmallOperand
                 | VariableOperand
                 | Omitted

{- The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. -}

oneOperandBytecodes = [
  OP1_128, OP1_129, OP1_130, OP1_131, OP1_132, OP1_133, OP1_134, OP1_135,
  OP1_136, OP1_137, OP1_138, OP1_139, OP1_140, OP1_141, OP1_142, OP1_143 ]

zeroOperandBytecodes = [
  OP0_176, OP0_177, OP0_178, OP0_179, OP0_180, OP0_181, OP0_182, OP0_183,
  OP0_184, OP0_185, OP0_186, OP0_187, OP0_188, OP0_189, OP0_190, OP0_191 ]

twoOperandBytecodes = [
  ILLEGAL, OP2_1,  OP2_2,  OP2_3,  OP2_4,  OP2_5,   OP2_6,   OP2_7,
  OP2_8,   OP2_9,  OP2_10, OP2_11, OP2_12, OP2_13,  OP2_14,  OP2_15,
  OP2_16,  OP2_17, OP2_18, OP2_19, OP2_20, OP2_21,  OP2_22,  OP2_23,
  OP2_24,  OP2_25, OP2_26, OP2_27, OP2_28, ILLEGAL, ILLEGAL, ILLEGAL ]

varOperandBytecodes = [
  VAR_224, VAR_225, VAR_226, VAR_227, VAR_228, VAR_229, VAR_230, VAR_231,
  VAR_232, VAR_233, VAR_234, VAR_235, VAR_236, VAR_237, VAR_238, VAR_239,
  VAR_240, VAR_241, VAR_242, VAR_243, VAR_244, VAR_245, VAR_246, VAR_247,
  VAR_248, VAR_249, VAR_250, VAR_251, VAR_252, VAR_253, VAR_254, VAR_255 ]

extBytecodes = [
  EXT_0,   EXT_1,   EXT_2,   EXT_3,   EXT_4,   EXT_5,   EXT_6,   EXT_7,
  EXT_8,   EXT_9,   EXT_10,  EXT_11,  EXT_12,  EXT_13,  EXT_14,  ILLEGAL,
  EXT_16,  EXT_17,  EXT_18,  EXT_19,  EXT_20,  EXT_21,  EXT_22,  EXT_23,
  EXT_24,  EXT_25,  EXT_26,  EXT_27,  EXT_28,  EXT_29,  ILLEGAL, ILLEGAL ]

--TODO: if blog9 doesn't work, check this and helper functions, else delete this comment
hasStore opcode ver =
  case opcode of
    OP1_143 -> Story.v4_OrLower ver  {- call_1n in v5, logical not in v1-4 -}
    OP0_181 -> Story.v4_OrHigher ver {- save branches in v3, stores in v4 -}
    OP0_182 -> Story.v4_OrHigher ver {- restore branches in v3, stores in v4 -}
    OP0_185 -> Story.v4_OrHigher ver {- pop in v4, catch in v5 -}
    VAR_233 -> ver == V6
    VAR_228 -> Story.v5_OrHigher ver
    _ -> opcode `elem` [OP2_8   , OP2_9   , OP2_15  , OP2_16  , OP2_17  , OP2_18  , OP2_19
                      , OP2_20  , OP2_21  , OP2_22  , OP2_23  , OP2_24  , OP2_25
                      , OP1_129 , OP1_130 , OP1_131 , OP1_132 , OP1_136 , OP1_142
                      , VAR_224 , VAR_231 , VAR_236 , VAR_246 , VAR_247 , VAR_248
                      , EXT_0   , EXT_1   , EXT_2   , EXT_3   , EXT_4   , EXT_9
                      , EXT_10  , EXT_19  , EXT_29]

hasText opcode =
  case opcode of
    OP0_178 -> True
    OP0_179 -> True
    _ -> False

hasBranch opcode ver =
  case opcode of
    OP0_181 -> Story.v3_OrLower ver {- save branches in v3, stores in v4 -}
    OP0_182 -> Story.v3_OrLower ver {- restore branches in v3, stores in v4 -}
    _ -> opcode `elem` [OP2_1   , OP2_2   , OP2_3   , OP2_4   , OP2_5   , OP2_6   , OP2_7   , OP2_10
                      , OP1_128 , OP1_129 , OP1_130 , OP0_189 , OP0_191
                      , VAR_247 , VAR_255
                      , EXT_6   , EXT_14 , EXT_24  , EXT_27]

opcodeName :: Bytecode -> Version -> String
opcodeName ILLEGAL _ = "ILLEGAL"
opcodeName OP2_1   _ = "je"
opcodeName OP2_2   _ = "jl"
opcodeName OP2_3   _ = "jg"
opcodeName OP2_4   _ = "dec_chk"
opcodeName OP2_5   _ = "inc_chk"
opcodeName OP2_6   _ = "jin"
opcodeName OP2_7   _ = "test"
opcodeName OP2_8   _ = "or"
opcodeName OP2_9   _ = "and"
opcodeName OP2_10  _ = "test_attr"
opcodeName OP2_11  _ = "set_attr"
opcodeName OP2_12  _ = "clear_attr"
opcodeName OP2_13  _ = "store"
opcodeName OP2_14  _ = "insert_obj"
opcodeName OP2_15  _ = "loadw"
opcodeName OP2_16  _ = "loadb"
opcodeName OP2_17  _ = "get_prop"
opcodeName OP2_18  _ = "get_prop_addr"
opcodeName OP2_19  _ = "get_next_prop"
opcodeName OP2_20  _ = "add"
opcodeName OP2_21  _ = "sub"
opcodeName OP2_22  _ = "mul"
opcodeName OP2_23  _ = "div"
opcodeName OP2_24  _ = "mod"
opcodeName OP2_25  _ = "call_2s"
opcodeName OP2_26  _ = "call_2n"
opcodeName OP2_27  _ = "set_colour"
opcodeName OP2_28  _ = "throw"
opcodeName OP1_128 _ = "jz"
opcodeName OP1_129 _ = "get_sibling"
opcodeName OP1_130 _ = "get_child"
opcodeName OP1_131 _ = "get_parent"
opcodeName OP1_132 _ = "get_prop_len"
opcodeName OP1_133 _ = "inc"
opcodeName OP1_134 _ = "dec"
opcodeName OP1_135 _ = "print_addr"
opcodeName OP1_136 _ = "call_1s"
opcodeName OP1_137 _ = "remove_obj"
opcodeName OP1_138 _ = "print_obj"
opcodeName OP1_139 _ = "ret"
opcodeName OP1_140 _ = "jump"
opcodeName OP1_141 _ = "print_paddr"
opcodeName OP1_142 _ = "load"
opcodeName OP1_143 ver = if Story.v4_OrLower ver then "not" else "call_1n"
opcodeName OP0_176 _ = "rtrue"
opcodeName OP0_177 _ = "rfalse"
opcodeName OP0_178 _ = "print"
opcodeName OP0_179 _ = "print_ret"
opcodeName OP0_180 _ = "nop"
opcodeName OP0_181 _ = "save"
opcodeName OP0_182 _ = "restore"
opcodeName OP0_183 _ = "restart"
opcodeName OP0_184 _ = "ret_popped"
opcodeName OP0_185 ver = if Story.v4_OrLower ver then "pop" else "catch"
opcodeName OP0_186 _ = "quit"
opcodeName OP0_187 _ = "new_line"
opcodeName OP0_188 _ = "show_status"
opcodeName OP0_189 _ = "verify"
opcodeName OP0_190 _ = "EXTENDED"
opcodeName OP0_191 _ = "piracy"
opcodeName VAR_224 ver = if Story.v3_OrLower ver then "call" else "call_vs"
opcodeName VAR_225 _ = "storew"
opcodeName VAR_226 _ = "storeb"
opcodeName VAR_227 _ = "put_prop"
opcodeName VAR_228 ver = if Story.v4_OrLower ver then "sread" else "aread"
opcodeName VAR_229 _ = "print_char"
opcodeName VAR_230 _ = "print_num"
opcodeName VAR_231 _ = "random"
opcodeName VAR_232 _ = "push"
opcodeName VAR_233 _ = "pull"
opcodeName VAR_234 _ = "split_window"
opcodeName VAR_235 _ = "set_window"
opcodeName VAR_236 _ = "call_vs2"
opcodeName VAR_237 _ = "erase_window"
opcodeName VAR_238 _ = "erase_line"
opcodeName VAR_239 _ = "set_cursor"
opcodeName VAR_240 _ = "get_cursor"
opcodeName VAR_241 _ = "set_text_style"
opcodeName VAR_242 _ = "buffer_mode"
opcodeName VAR_243 _ = "output_stream"
opcodeName VAR_244 _ = "input_stream"
opcodeName VAR_245 _ = "sound_effect"
opcodeName VAR_246 _ = "read_char"
opcodeName VAR_247 _ = "scan_table"
opcodeName VAR_248 _ = "not"
opcodeName VAR_249 _ = "call_vn"
opcodeName VAR_250 _ = "call_vn2"
opcodeName VAR_251 _ = "tokenise"
opcodeName VAR_252 _ = "encode_text"
opcodeName VAR_253 _ = "copy_table"
opcodeName VAR_254 _ = "print_table"
opcodeName VAR_255 _ = "check_arg_count"
opcodeName EXT_0   _ = "save"
opcodeName EXT_1   _ = "restore"
opcodeName EXT_2   _ = "log_shift"
opcodeName EXT_3   _ = "art_shift"
opcodeName EXT_4   _ = "set_font"
opcodeName EXT_5   _ = "draw_picture"
opcodeName EXT_6   _ = "picture_data"
opcodeName EXT_7   _ = "erase_picture"
opcodeName EXT_8   _ = "set_margins"
opcodeName EXT_9   _ = "save_undo"
opcodeName EXT_10  _ = "restore_undo"
opcodeName EXT_11  _ = "print_unicode"
opcodeName EXT_12  _ = "check_unicode"
opcodeName EXT_13  _ = "set_true_colour"
opcodeName EXT_14  _ = "sound_data"
opcodeName EXT_16  _ = "move_window"
opcodeName EXT_17  _ = "window_size"
opcodeName EXT_18  _ = "window_style"
opcodeName EXT_19  _ = "get_wind_prop"
opcodeName EXT_20  _ = "scroll_window"
opcodeName EXT_21  _ = "pop_stack"
opcodeName EXT_22  _ = "read_mouse"
opcodeName EXT_23  _ = "mouse_window"
opcodeName EXT_24  _ = "push_stack"
opcodeName EXT_25  _ = "put_wind_prop"
opcodeName EXT_26  _ = "print_form"
opcodeName EXT_27  _ = "make_menu"
opcodeName EXT_28  _ = "picture_table"
opcodeName EXT_29  _ = "buffer_screen"

display :: Instruction.T -> Version -> String
display instr ver =
  let startAddr = (address instr) :: InstructionAddress in
  let name = opcodeName (opcode instr) ver in
  let operands = displayOperands() in
  let store = displayStore() in
  let branch = displayBranch() in
  let text = displayText() in
    printf "%04x: %s %s%s %s %s\n" startAddr name operands store branch text
      where
        displayOperands () = accumulateStrings toString (operands instr)

        toString :: Operand -> String
        toString operand =
          case operand of
            (Large large) -> (printf "%04x " large)
            (Small small) -> (printf "%02x " small)
            (Variable variable) -> ((displayVariable variable) ++ " ")
        
        displayStore () =
          case store instr of Nothing -> ""
                              Just variable -> "->" ++ (displayVariable variable)
        displayBranch () =
          case branch instr of Nothing -> ""
                               Just (True, ReturnFalse) -> "?false"
                               Just (False, ReturnFalse) -> "?~false"
                               Just (True, ReturnTrue) -> "?true"
                               Just (False, ReturnTrue) -> "?~true"
                               Just (True, BranchAddress (Instruction address)) -> printf "?%04x" address
                               Just (False, BranchAddress (Instruction address)) -> printf "?~%04x" address
        displayText () =
          case text instr of Nothing -> ""
                             Just str -> str
{- End of display_instruction -}



{- Takes the address of an instruction and produces the instruction -}
decode :: Story.T -> InstructionAddress -> Instruction.T
decode story (Instruction address) =
  let form = decodeForm addr in
  let opCount = decodeOpCount addr form in
  let opcode = decodeOpcode addr form opCount in
  let opcodeLength = getOpcodeLength form in
  let operandTypes = decodeOperandTypes addr form opCount opcode in
  let typeLength = getTypeLength form opcode in
  let operandAddress = incByteAddrBy addr (opcodeLength + typeLength) in
  let operands = decodeOperands operandAddress operandTypes in
  let operandLength = getOperandLength operandTypes in
  let storeAddress = incByteAddrBy operandAddress operandLength in
  let store = decodeStore storeAddress opcode ver in
  let storeLength = getStoreLength opcode ver in
  let branchCodeAddress = incByteAddrBy storeAddress storeLength in
  let branch = decodeBranch branchCodeAddress opcode ver in
  let branchLength = getBranchLength branchCodeAddress opcode ver in
  let (ByteAddress ba) = branchCodeAddress in
  let textAddress = Zstring (ba + branchLength) in
  let text = decodeText textAddress opcode in
  let textLength = getTextLength textAddress opcode in
  let len =
        opcodeLength + typeLength + operandLength + storeLength +
        branchLength + textLength in
  let address' = Instruction address in
          Instruction.T { opcode = opcode, address = address', Instruction.length = len, operands = operands, store = store, branch = branch, text = text }
      where
        addr = ByteAddress address
        ver = (Story.version story)
        readWord = Story.readWord story
        readByte = Story.readByte story
        readZstring = Zstring.read story
        zstringLength = Zstring.length story

        {- Spec 4.3:

        Each instruction has a form (long, short, extended or variable)  ...
        If the top two bits of the opcode are $$11 the form is variable;
        if $$10, the form is short. If the opcode is 190 ($BE in hexadecimal)
        and the version is 5 or later, the form is "extended". Otherwise,
        the form is "long". -}

        decodeForm address =
          let b = readByte address in
          case fetchBits bit7 size2 b of 3 -> VariableForm
                                         2 -> if b == 190 then ExtendedForm else ShortForm
                                         _ -> LongForm

        {- Spec:
        * Each instruction has ... an operand count (0OP, 1OP, 2OP or VAR).
        * In short form, bits 4 and 5 of the opcode byte ... If this is $11
          then the operand count is 0OP; otherwise, 1OP.
        * In long form the operand count is always 2OP.
        * In variable form, if bit 5 is 0 then the count is 2OP; if it is 1,
          then the count is VAR.
        * In extended form, the operand count is VAR. -}

        decodeOpCount address form =
          let b = readByte address in
          case form of ShortForm -> if fetchBits bit5 size2 b == 3 then OP0 else OP1
                       LongForm -> OP2
                       VariableForm -> if fetchBit bit5 b then VAR else OP2
                       ExtendedForm -> VAR

        {- Spec :
        * In short form, ... the opcode number is given in the bottom 4 bits.
        * In long form ... the opcode number is given in the bottom 5 bits.
        * In variable form, ... the opcode number is given in the bottom 5 bits.
        * In extended form, ... the opcode number is given in a second opcode byte. -}

        {- Now what the spec does not say here clearly is: we have just read 4, 5 or
           8 bits, but we need to figure out which of 100+ opcodes we're talking
           about. The location of the bits depends on the form, but the meaning of
           of the bits depends on the operand count. In fact the operation count
           is far more relevant here. It took me some time to puzzle out this
           section of the spec. The spec could more clearly say:

         * In extended form the EXT opcode number is given in the following byte. Otherwise:
         * If the operand count is 0OP then the 0OP opcode number is given in
           the lower 4 bits.
         * If the operand count is 1OP then the 1OP opcode number is given in
           the lower 4 bits.
         * if the operand count is 2OP then the 2OP opcode number is given in
           the lower 5 bits
         * If the operand count is VAR then the VAR opcode number is given in
           the lower 5 bits
        -}

        decodeOpcode address form opCount =
          let b = readByte address in
          case (form, opCount) of (ExtendedForm, _) ->
                                    let maximumExtended = 29 in
                                    let ext = readByte (incByteAddr address) in
                                    if ext > maximumExtended then ILLEGAL else extBytecodes !! ext
                                  (_, OP0) -> zeroOperandBytecodes !! (fetchBits bit3 size4 b)
                                  (_, OP1) -> oneOperandBytecodes !! (fetchBits bit3 size4 b)
                                  (_, OP2) -> twoOperandBytecodes !! (fetchBits bit4 size5 b)
                                  (_, VAR) -> varOperandBytecodes !! (fetchBits bit4 size5 b)

        getOpcodeLength form =
          case form of ExtendedForm -> 2
                       _ -> 1

        {- Spec:
        There are four 'types' of operand. These are often specified by a
        number stored in 2 binary digits:
        * $$00 Large constant (0 to 65535) 2 bytes
        * $$01 Small constant (0 to 255) 1 byte
        * $$10 Variable 1 byte
        * $$11 Omitted altogether 0 bytes -}

        decodeTypes n =
          case n of 0 -> LargeOperand
                    1 -> SmallOperand
                    2 -> VariableOperand
                    _ -> Omitted

        {- Spec 4.4
        Next, the types of the operands are specified.
        * In short form, bits 4 and 5 of the opcode give the type.
        * In long form, bit 6 of the opcode gives the type of the first operand,
          bit 5 of the second. A value of 0 means a small constant and 1 means a
          variable.
        * In variable or extended forms, a byte of 4 operand types is given next.
          This contains 4 2-bit fields: bits 6 and 7 are the first field, bits 0 and
          1 the fourth. The values are operand types as above. Once one type has
          been given as 'omitted', all subsequent ones must be.
        * In the special case of the "double variable" VAR opcodes call_vs2 and
          call_vn2 a second byte of types is given, containing the types for the
          next four operands. -}

        {- Once again this could be more clearly written; the spec never calls
           out for instance the obvious fact that 0OP codes have no operand types.
           The logic is:

        * If the count is 0OP then there are no operand types.
        * If the count is 1OP then bits 4 and 5 of the opcode give the type
        * In long form the count is 2OP; bit 6 ... -}

        {- We walk the byte from low bit pairs -- which correspond to later
           operands -- to high bit pairs, so that the resulting list has
           the first operands at the head and last at the tail -}
        decodeVariableTypes typeByte =
          aux 0 []
          where
            aux i acc =
              if i > 3 then
                acc
              else
                let typeBits = fetchBits (BitNumber (i * 2 + 1)) size2 typeByte in
                case decodeTypes typeBits of Omitted -> aux (i + 1) acc
                                             x -> aux (i + 1) (x : acc)

        decodeOperandTypes address form opCount opcode =
          case (form, opCount, opcode) of
            (_, OP0, _) -> []
            (_, OP1, _) ->
              let b = readByte address in
                [decodeTypes (fetchBits bit5 size2 b)]
            (LongForm, _, _) ->
              let b = readByte address in
                (case fetchBits bit6 size2 b of
                  0 -> [ SmallOperand, SmallOperand ]
                  1 -> [ SmallOperand, VariableOperand ]
                  2 -> [ VariableOperand, SmallOperand ]
                  _ -> [ VariableOperand, VariableOperand ])
            (VariableForm, _, VAR_236) -> concatDecodedVariables() -- can we fall through?
            (VariableForm, _, VAR_250) -> concatDecodedVariables()
            _ ->
              let opcodeLength = getOpcodeLength form in
              let typeByte = readByte (incByteAddrBy address opcodeLength) in
                decodeVariableTypes typeByte
            where
              concatDecodedVariables () =
                      let opcodeLength = getOpcodeLength form in
                      let typeByte0 = readByte (incByteAddrBy address opcodeLength) in
                      let typeByte1 = readByte (incByteAddrBy address (opcodeLength + 1)) in
                        (decodeVariableTypes typeByte0) ++ (decodeVariableTypes typeByte1)

        getTypeLength form opcode =
          case (form, opcode) of (VariableForm, VAR_236) -> 2 -- can we fall through?
                                 (VariableForm, VAR_250) -> 2
                                 (VariableForm, _) -> 1
                                 _ -> 0

        {- The operand types are large, small or variable, being 2, 1 and 1 bytes
           respectively. We take the list of operand types and produce a list of
           operands. -}

        {- This method is not tail recursive but the maximum number of operands
           is eight, so we don't care. -}
        decodeOperands operandAddress operandTypes =
          case operandTypes of
            [] -> []
            (LargeOperand : remainingTypes) ->
              let w = readWord (byteAddrToWordAddr operandAddress) in
              let tail' = decodeOperands (incByteAddrBy operandAddress wordSize) remainingTypes in
                (Large w) : tail'
            (SmallOperand : remainingTypes) ->
              let b = readByte operandAddress in
              let tail' = decodeOperands (incByteAddr operandAddress) remainingTypes in
                (Small b) : tail'
            (VariableOperand : remainingTypes) ->
              let b = readByte operandAddress in
              let v = decodeVariable b in
              let tail' = decodeOperands (incByteAddr operandAddress) remainingTypes in
                (Variable v) : tail'
            Omitted : _ -> error "omitted operand type passed to decode operands"

        getOperandLength operandTypes =
          case operandTypes of
            [] -> 0
            (LargeOperand : remainingTypes) -> wordSize + (getOperandLength remainingTypes)
            _ : remainingTypes -> 1 + (getOperandLength remainingTypes)
            -- verify the above line (remaining types) is a correct translation

        {- Spec 4.6:
        "Store" instructions return a value: e.g., mul multiplies its two
        operands together. Such instructions must be followed by a single byte
        giving the variable number of where to put the result. -}

        {- This is straightforward but I note something odd; the wording above
          implies that the instruction has ended after the operands, and that
          the store (and hence also branch and text) *follow* the instruction.
          I cannot get behind this. The store, branch and text are all part of
          an instruction. -}

        decodeStore storeAddress opcode ver =
          if hasStore opcode ver then
            let storeByte = readByte storeAddress in
            Just (decodeVariable storeByte)
          else
            Nothing

        getStoreLength opcode ver =
          if hasStore opcode ver then 1 else 0

        {- Spec 4.7
        * Instructions which test a condition are called "branch" instructions.
        * The branch information is stored in one or two bytes, indicating what to
          do with the result of the test.
        * If bit 7 of the first byte is 0, a branch occurs when the condition was
          false; if 1, then branch is on true.
        * If bit 6 is set, then the branch occupies 1 byte only, and the "offset"
          is in the range 0 to 63, given in the bottom 6 bits.
        * If bit 6 is clear, then the offset is a signed 14-bit number given in
          bits 0 to 5 of the first byte followed by all 8 of the second.
        * An offset of 0 means "return false from the current routine", and 1 means
          "return true from the current routine".
        * Otherwise, a branch moves execution to the instruction at address
          (Address after branch data) + Offset - 2. -}

        decodeBranch branchCodeAddress opcode ver  =
          if hasBranch opcode ver then
            let high = readByte branchCodeAddress in
            let sense = fetchBit bit7 high in
            let bottom6 = fetchBits bit5 size6 high in
            let offset =
                  if fetchBit bit6 high then
                    bottom6
                  else
                    let low = readByte (incByteAddr branchCodeAddress) in
                    let unsigned = 256 * bottom6 + low in
                    if unsigned < 8192 then unsigned else unsigned - 16384 in
            let branch =
                  case offset of
                    0 -> (sense, ReturnFalse)
                    1 -> (sense, ReturnTrue)
                    _ ->
                      let branchLength = if fetchBit bit6 high then 1 else 2 in
                      let (ByteAddress addressAfter) = incByteAddrBy branchCodeAddress branchLength in
                      let branchTarget = Instruction (addressAfter + offset - 2) in
                      (sense, BranchAddress branchTarget) in
            Just branch
          else
            Nothing

        getBranchLength branchCodeAddress opcode ver =
          if hasBranch opcode ver then
            let b = readByte branchCodeAddress in
            if fetchBit bit6 b then 1 else 2
          else 0

        {- Spec:
          Two opcodes, print and print_ret, are followed by a text string. -}

        decodeText textAddress opcode =
          if hasText opcode then
            Just (readZstring textAddress)
          else
            Nothing

        getTextLength textAddress opcode =
          if hasText opcode then
            zstringLength textAddress
          else
            0

