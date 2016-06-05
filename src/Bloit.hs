module Bloit where

import Data.Bits
import ImmutableBytes
import Text.Printf
import Type
import Utility

main :: IO()
main = let addr1 = ByteAddress 1 in
        let bytes_a = ImmutableBytes.make "Hello world" in
          let bytes_b = ImmutableBytes.writeByte bytes_a addr1 65 in
            let b_a = ImmutableBytes.readByte bytes_a addr1 in
              let b_b = ImmutableBytes.readByte bytes_b addr1 in
       printf "%d %d\n" b_a b_b