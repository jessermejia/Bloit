module Bloit where

import Object
import Text.Printf
import Story

main :: IO ()
main = do
  story <- Story.load "minizork.z3"
  let table = Object.displayObjectTable story in
    printf "%s\n" table