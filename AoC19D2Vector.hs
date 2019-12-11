module AoC19D2Vector where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as V


        

--initialises the IntCode and then runs the program
--opCode 99 ends the program
runIntCode :: (Int,Int) -> Vector Int -> Vector Int
runIntCode nounverb program = compute (initialise nounverb program) 0
    where
      compute program pointer
          | opCode == 99 = program
          | opCode == 1 = compute (runOpCode1 program pointer) (pointer + 4)
          | opCode == 2 = compute (runOpCode2 program pointer) (pointer + 4)
          where
            opCode = program V.! pointer
            
-- function to initialise the program with a given noun and verb
-- the noun replaces the value at position 1
-- the verb replaces the value at postition 2
initialise :: (Int,Int) -> Vector Int -> Vector Int    
initialise (noun,verb) program = program V.// [(1,noun),(2,verb)]

--Opcode 1 takes the values of the positions indicated by the next two integers
--adds them and stores them at the position indicated bt the third integer
runOpCode1 :: Vector Int -> Int -> Vector Int
runOpCode1 program pointer = program V.// [(storage,x+y)]
    where
        x = (V.!) program $ program V.! (pointer+1)
        y = (V.!) program $ program V.! (pointer+2)
        storage = program V.! (pointer+3)

--Opcode 1 takes the values of the positions indicated by the next two integers
--mulitplies them and stores them at the position indicated bt the third integer        
runOpCode2 :: Vector Int -> Int -> Vector Int
runOpCode2 program pointer = program V.// [(storage,x*y)]
    where
        x = (V.!) program $ program V.! (pointer+1)
        y = (V.!) program $ program V.! (pointer+2)
        storage = program V.! (pointer+3)        

    
--
-- Tests and input
--
testProgram4 :: Vector Int
testProgram4 = V.fromList [1,1,1,4,99,5,6,0,99]
--30,1,1,4,2,5,6,0,99

testProgram3 :: Vector Int
testProgram3 = V.fromList [2,4,4,5,99,0]
--2,4,4,5,99,9801

testProgram2 :: Vector Int
testProgram2 = V.fromList [2,3,0,3,99]
-- 2,3,0,6,99

testProgram1 :: Vector Int
testProgram1 = V.fromList [1,0,0,0,99]
-- 2,0,0,0,99

inputProgram :: Vector Int
inputProgram = V.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23
    ,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1
    ,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10
    ,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13
    ,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123
    ,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0
    ,14,0]