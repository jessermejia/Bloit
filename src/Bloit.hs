module Bloit where

import Object
import Text.Printf
import Story

main :: IO ()
main = do
  story <- Story.load "minizork.z3"
  let tree = Object.displayObjectTree story in
    printf "%s\n" tree