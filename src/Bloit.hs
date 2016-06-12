module Bloit where

import Dictionary
import Text.Printf
import Story
import Type
import Zstring

main :: IO ()
main = do
  story <- Story.load "minizork.z3"
  let dict = Dictionary.display story in
    printf "%s\n" dict
