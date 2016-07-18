module EvaluationStack where

import           Data.List
import           Text.Printf
import           Utility

data T = T {
            items :: [Int]
           }

empty :: EvaluationStack.T
empty = T { items = [] }

length :: EvaluationStack.T -> Int
length stack =
  Data.List.length (items stack)

peek :: EvaluationStack.T -> Int
peek stack =
  case items stack of
    [] -> error "peeking an empty stack"
    (x:_) -> x

pop :: EvaluationStack.T -> EvaluationStack.T
pop stack =
  case items stack of
    [] -> error "popping empty stack"
    (_:xs) -> T { items = xs }

push :: EvaluationStack.T -> Int -> EvaluationStack.T
push stack item =
  let item = unsignedWord item in
  T { items = item : items stack }

display :: EvaluationStack.T -> String
display stack =
  accumulateStrings toString (items stack)
  where
    toString item =
      printf " %04x" item
