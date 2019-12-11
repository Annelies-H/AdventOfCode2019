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
          | otherwise = V.empty
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
runOpCode1 program pointer = program V.// [(param3 , param1 + param2)]
    where
        param1 = (V.!) program $ program V.! (pointer+1)
        param2 = (V.!) program $ program V.! (pointer+2)
        param3 = program V.! (pointer+3)

--Opcode 1 takes the values of the positions indicated by the next two integers
--mulitplies them and stores them at the position indicated bt the third integer        
runOpCode2 :: Vector Int -> Int -> Vector Int
runOpCode2 program pointer = program V.// [(param3 ,param1 * param2)]
    where
        param1 = (V.!) program $ program V.! (pointer+1)
        param2 = (V.!) program $ program V.! (pointer+2)
        param3 = program V.! (pointer+3)        

    
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

