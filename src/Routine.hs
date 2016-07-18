module Routine where

import           Story
import           Type
import           Utility

localsCount :: Story.T -> RoutineAddress -> Int
localsCount story (Routine routineAddress) =
  Story.readByte story (ByteAddress routineAddress)

firstInstruction :: Story.T -> RoutineAddress -> InstructionAddress
firstInstruction story (Routine routineAddress) =
  if Story.v4_OrLower (Story.version story) then
    let count = localsCount story (Routine routineAddress) in
    Instruction (routineAddress + 1 + count * wordSize)
  else
    Instruction (routineAddress + 1)

localDefaultValue :: Story.T -> RoutineAddress -> Int -> Int
localDefaultValue story (Routine routineAddress) n =
  if Story.v4_OrLower (Story.version story) then
    let addr = WordAddress(routineAddress + 1) in
    Story.readWord story (incWordAddrBy addr (n - 1))
  else
    0
