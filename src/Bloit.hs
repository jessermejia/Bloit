module Bloit where

import Instruction
import Object
import Text.Printf
import Story
import Type

main :: IO ()
main = do
--   story <- Story.load "minizork.z3"
--   let tree = Object.displayObjectTree story in
--     printf "%s\n" tree

  story <- Story.load "minizork.z3"
  let instruction = Instruction.decode story (Instruction 0x37d9) in
    let text = Instruction.display instruction (Story.version story) in
      printf "%s\n" text