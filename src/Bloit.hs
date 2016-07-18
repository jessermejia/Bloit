module Bloit where

-- import Dictionary
-- import ImmutableBytes
-- import Instruction
import           LocalStore
-- import Object
import           Reachability
import           Story
import           Text.Printf
import           Type
-- import Utility
-- import Zstring

main :: IO ()
main = do

-- BLOG #1
  -- 3.2 word definition: http://inform-fiction.org/zmachine/standards/z1point1/sect03.html
  -- 0xBEEF == 1011 1110 1110 1111
  -- bit0 is the right most bit (least significant)
  -- bit 15 is the left most bit (most significant)
  -- "I want 4 bits from this word, starting from bit 15 (going down),"
  -- the following pulls: 1011
  -- 1011 == B (or b) in hexadecimal
  -- see the unit tests in UtilitySpec for better detail on how this works
    -- let word = 0xBEEF :: Int
    --     bitNumber = BitNumber 15
    --     bitSize = BitSize 4 in
    --       printf "%0x\n" (fetchBits bitNumber bitSize word)


-- BLOG #2
  -- here we are demonstrating that:
  -- a) we can make a string "Hello world"
  -- b) we can read ByteAddress 1 ('e')
  -- c) we can "write" that same value
  -- d) that our data structure is immutable
  -- let addr1 = ByteAddress 1 in
  --   let bytesA = ImmutableBytes.make "Hello world" in
  --     let bytesB = ImmutableBytes.writeByte bytesA addr1 65 in
  --       let bA = ImmutableBytes.readByte bytesA addr1 in
  --         let bB = ImmutableBytes.readByte bytesB addr1 in
  --           printf "%d %d\n" bA bB

-- BLOG #3
  -- This proves that we can read in the version address of a story file
  -- .z3 is supposed to be a version 3 story file
    -- story <- Story.load "minizork.z3"
    -- let versionAddress = ByteAddress 0 in
    --   let version = Story.readByte story versionAddress in
    --       printf "%d\n" version

-- BLOG #4
  -- pulling out commonly used words from the abbreviations section
  -- from the story file.
  -- Not working...had same problem the first time around, can't
  -- remember how i fixed it :'(
  -- the output should be:
  -- 19 t 0d h 0a e 00 _ 05 ? 05 ?
  -- 1e y 14 o 1a u 17 r 00 _ 05 ?
  -- we can see that 'e' corresponds to 0a (or A) in Zstring.alphabetTable
    -- story <- Story.load "minizork.z3"
    -- let zstring = Zstring.abbreviationZstring story (Abbreviation 0) in
    --     let text = Zstring.displayBytes story zstring in
    --     printf "%s\n" text
    -- let zstring = Zstring.abbreviationZstring story (Abbreviation 4) in
    --   let text' = Zstring.displayBytes story zstring in
    --   printf "%s\n" text'

-- BLOG #5
  -- testing out the zstring decoder
  -- eric lippert happened to know that there was a string at 0xb106
  -- which is why we are using it below
    -- story <- Story.load "minizork.z3"
    -- let zstring = Zstring 0xb106 in
    --   let text = Zstring.read story zstring in
    --     printf "%s\n" text;

-- BLOG #6
  -- apparently this lets us see every word meaningful in Mini-Zork
  -- (The $ve, #rand, and so on words allow the developers to tweak
  -- aspects of game play for testing. Re-seeding the random number
  -- generator, for instance.)
    -- story <- Story.load "minizork.z3"
    -- let dict = Dictionary.display story in
    --   printf "%s\n" dict

-- BLOG #7
  -- this gives us the object table, but it doesn't show order
  -- or that objects are actually contained in a tree data structure
    -- story <- Story.load "minizork.z3"
    -- let table = Object.displayObjectTable story in
    --     printf "%s\n" table

-- BLOG #8
  -- this lets us see the tree structure
  -- you can see that some objects contain other objects
  -- an explanation of objects and attributes (pretty interesting):
  -- http://inform-fiction.org/zmachine/standards/z1point1/overview.html
    -- story <- Story.load "minizork.z3"
    -- let tree = Object.displayObjectTree story in
    --   printf "%s\n" tree

-- BLOG #9
  -- this shows an instruction
  -- The instruction here is a call, it takes three long constant operands,
  -- and it stores its result by pushing it onto the evaluation stack.
  -- There is no branch, and no text.
    -- story <- Story.load "minizork.z3"
    -- let instruction = Instruction.decode story (Instruction 0x37d9) in
    --   let text' = Instruction.display instruction (Story.version story) in
    --     printf "%s\n" text'

-- BLOG #10
  -- story <- Story.load "minizork.z3"
  -- let text = Reachability.displayReachableInstructions story (Instruction 0x37d9) in
  --   printf "%s\n" text

-- BLOG #11
  -- story <- Story.load "minizork.z3"
  -- let text = Reachability.displayReachableInstructions story (Instruction 0x381d) in
  --   printf "%s\n" text

-- BLOG #12
  story <- Story.load "minizork.z3"
  let locals = LocalStore.createDefaultLocals story (Routine 0x3b36)
      text = LocalStore.display locals in
        printf "%s\n" text
