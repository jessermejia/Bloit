module Reachability where

import           Data.List
import           Instruction
import           Story
import           Type
import           Utility

{- Any given instruction in a routine either goes on to the next instruction,
when it is done, or branches to another instruction when it is done, or terminates
the routine. Given the address of an instruction, what are all the reachable instructions
in this routine? Note that this could miss instructions if a jump is made to a location
read from a variable. -}

followingInstruction :: Instruction.T -> [InstructionAddress]
followingInstruction instr =
  if Instruction.continuesToFollowing (Instruction.opcode instr) then
    let Instruction addr = Instruction.address instr in
    let len = Instruction.length instr in
    [Instruction (addr + len)]
  else
    []

branchTargetInstruction :: Instruction.T -> [InstructionAddress]
branchTargetInstruction instr =
  case Instruction.branch instr of
    Nothing -> []
    Just (_, ReturnFalse) -> []
    Just (_, ReturnTrue) -> []
    Just (_, BranchAddress address') -> [address']

jumpTargetInstruction :: Instruction.T -> [InstructionAddress]
jumpTargetInstruction instr =
  case (Instruction.opcode instr, Instruction.operands instr) of
    (OP1_140, [Large offset']) ->
      let offset = signedWord offset' in
      [ Instruction.jumpAddress instr offset ]
    _ -> []

allReachableAddressesInRoutine :: Story.T -> InstructionAddress -> [InstructionAddress]
allReachableAddressesInRoutine story instrAddress =
  reflexiveClosure instrAddress immediatelyReachableAddresses
  where
    immediatelyReachableAddresses address' =
      let instr = Instruction.decode story address' in
      let following' = followingInstruction instr in
      let branch' = branchTargetInstruction instr in
      let jump = jumpTargetInstruction instr in
      following' ++ branch' ++ jump

displayReachableInstructions :: Story.T -> InstructionAddress ->  String
displayReachableInstructions story address' =
  let reachable = allReachableAddressesInRoutine story address' in
  let sorted = sort reachable in
  accumulateStrings toString sorted
  where
    toString addr =
      let instr = Instruction.decode story addr in
      Instruction.display instr (Story.version story)
