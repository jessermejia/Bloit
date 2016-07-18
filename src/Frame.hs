module Frame where

import           EvaluationStack
import           LocalStore
import           Text.Printf
import           Type
import           Utility

data T = T {  stack      :: EvaluationStack.T
            , localStore :: LocalStore.T
            , resumeAt   :: InstructionAddress
            , store      :: Maybe VariableLocation
           }

empty :: Frame.T
empty = Frame.T { stack = EvaluationStack.empty
                , localStore = LocalStore.empty
                , resumeAt = Instruction 0
                , store = Nothing
                }

peekStack :: Frame.T -> Int
peekStack frame =
  EvaluationStack.peek (stack frame)

popStack :: Frame.T -> Frame.T
popStack frame =
  frame { stack = EvaluationStack.pop (stack frame) }

pushStack :: Frame.T -> Int -> Frame.T
pushStack frame value =
  frame { stack = EvaluationStack.push (stack frame) value }

writeLocal :: Frame.T -> LocalVariable -> Int -> Frame.T
writeLocal frame local value =
  frame { localStore = LocalStore.writeLocal (localStore frame) local value }

readLocal :: Frame.T -> LocalVariable -> Maybe Int
readLocal frame local =
  LocalStore.readLocal (localStore frame) local

display :: Frame.T -> String
display frame =
  let (Instruction resumeAt') = resumeAt frame
      locals = LocalStore.display (localStore frame)
      stack' = EvaluationStack.display (stack frame)  in
        printf "Locals %s\nStack %s\nResume at:%04x\n"
  locals stack' resumeAt'
