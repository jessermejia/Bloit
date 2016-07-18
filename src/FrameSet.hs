module FrameSet where
{- The frame set is the stack of activation frames; each activation frame
has a local variable storage, evaluation stack, and information about
the call site that created this activation. -}

import           Frame
import           Type
import           Utility

data T = T {  initialFrame :: Frame.T
           ,  frames       :: [Frame.T]
           }

make :: Frame.T -> FrameSet.T
make initialFrame =
  FrameSet.T { initialFrame = initialFrame, frames = []}

currentFrame :: FrameSet.T -> Frame.T
currentFrame frameset =
  case frames frameset of
    [] -> initialFrame frameset
    (x:_) -> x

setCurrentFrame :: FrameSet.T -> Frame.T -> FrameSet.T
setCurrentFrame frameset frame =
  case frames frameset of
    [] -> frameset { initialFrame = frame }
    (_:xs) -> frameset { frames = frame : xs }

addFrame :: FrameSet.T -> Frame.T -> FrameSet.T
addFrame frameset frame =
  frameset { frames = frame : frames frameset }

removeFrame :: FrameSet.T -> FrameSet.T
removeFrame frameset =
  case frames frameset of
    [] -> error "Attempting to remove initial frame"
    (_:xs) -> frameset { frames = xs }

peekStack :: FrameSet.T -> Int
peekStack frameset =
  Frame.peekStack (currentFrame frameset)

popStack :: FrameSet.T -> FrameSet.T
popStack frameset =
  setCurrentFrame frameset (Frame.popStack (currentFrame frameset))

pushStack :: FrameSet.T -> Int -> FrameSet.T
pushStack frameset value =
  setCurrentFrame frameset (Frame.pushStack (currentFrame frameset) value)

readLocal :: FrameSet.T -> LocalVariable -> Maybe Int
readLocal frameset local =
  Frame.readLocal (currentFrame frameset) local

writeLocal :: FrameSet.T -> LocalVariable -> Int -> FrameSet.T
writeLocal frameset local value =
  setCurrentFrame frameset (Frame.writeLocal (currentFrame frameset) local value)

display :: FrameSet.T -> String
display frameset =
  accumulateStrings Frame.display (frames frameset) ++ Frame.display (initialFrame frameset)
