module Main where

import Lib
import Data.Array.Accelerate.LLVM.Native as CPU

main :: IO ()
main = print (CPU.run res)
