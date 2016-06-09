module Bloit where

import Text.Printf
import Story
import Type
import Zstring

main :: IO ()
main = do
  story <- Story.load "src/minizork.z3"
  let zstring = Zstring.abbreviationZstring story (Abbreviation 0) in
    let text = Zstring.displayBytes story zstring in
    printf "%s\n" text
  let zstring = Zstring.abbreviationZstring story (Abbreviation 4) in
    let text = Zstring.displayBytes story zstring in
    printf "%s\n" text