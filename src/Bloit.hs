module Bloit where

import Text.Printf
import Story
import Type

main :: IO ()
main = do
  story <- Story.load "src/minizork.z3"
  let versionAddress = ByteAddress 0 in
    let version = Story.readByte story versionAddress in
      printf "%d\n" version