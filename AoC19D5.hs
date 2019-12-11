module AoC19D5 where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V


data Mode = Immediate | Position

instance Eq Mode where
    (==) Immediate Immediate = True
    (==) Position Position = True
    (==) _ _ = False

   

--initialises the IntCode and then runs the program
--opCode 99 ends the program
runIntCode :: (Int,Int) -> [Int] -> Vector Int -> [Int]
runIntCode nounverb input program = compute (initialise nounverb program) 0 input []
    where
      compute program pointer input output
          | opCode == 99 = output
          | opCode == 1 = compute (runOpCode1 program pointer instruction) (pointer + 4) input output
          | opCode == 2 = compute (runOpCode2 program pointer instruction) (pointer + 4) input output
          | opCode == 3 = compute (runOpCode3 program pointer instruction (head input)) (pointer + 2) (tail input) output
          | opCode == 4 = compute program (pointer + 2) input ((runOpCode4 program pointer instruction):output)
          | otherwise = []
          where
            opCode = head instruction
            instruction = reverse . getDigits $ program V.! pointer
            
-- function to initialise the program with a given noun and verb
-- the noun replaces the value at position 1
-- the verb replaces the value at postition 2
initialise :: (Int,Int) -> Vector Int -> Vector Int    
initialise (noun,verb) program = program V.// [(1,noun),(2,verb)]


-- function to create a list of all the digits in a number
-- where the last two digits are treated as one
-- used to get the modes (1 digit) and opcodes (2 digits)
getDigits :: Int -> [Int]
getDigits n = go (divMod n 100) []
    where
        go (0,m) result = m:result
        go (d,m) result = go (divMod d 10) (m:result)

-- function to get a mode based on a (reverse) list of all the digits in the instructionDigits
-- paramater = 1 is the 1st paramater = 3rd in the digits list
-- paramter = 2 is the 2nd parameter = 4th in the digits list
-- etc.        
getMode :: Int -> [Int] -> Mode      
getMode parameter instructionDigits
    | parameter >= length instructionDigits = Position
    | instructionDigits !! parameter == 1 = Immediate
    | instructionDigits !! parameter == 0 = Position

--Opcode 1 takes the values of its firs two paramaters
--adds them and stores them at its third paramater
runOpCode1 :: Vector Int -> Int -> [Int] -> Vector Int
runOpCode1 program pointer instruction = program V.// [(param3 , param1 + param2)]
    where
        param1
          | getMode 1 instruction == Position = (V.!) program $ program V.! (pointer+1)
          | getMode 1 instruction == Immediate = program V.! (pointer+1)
        param2
          | getMode 2 instruction == Position = (V.!) program $ program V.! (pointer+2)
          | getMode 2 instruction == Immediate = program V.! (pointer+2)
        param3
          | getMode 3 instruction == Position = program V.! (pointer+3)
          | getMode 3 instruction == Immediate = pointer+2

--Opcode 2 takes the values of its first two parameters
--mulitplies them and stores them at its third paramater
runOpCode2 :: Vector Int -> Int -> [Int] -> Vector Int
runOpCode2 program pointer instruction = program V.// [(param3 ,param1 * param2)]
    where
        param1
          | getMode 1 instruction == Position = (V.!) program $ program V.! (pointer+1)
          | getMode 1 instruction == Immediate = program V.! (pointer+1)
        param2
          | getMode 2 instruction == Position = (V.!) program $ program V.! (pointer+2)
          | getMode 2 instruction == Immediate = program V.! (pointer+2)
        param3
          | getMode 3 instruction == Position = program V.! (pointer+3)
          | getMode 3 instruction == Immediate = pointer+2       

--Opcode 3 takes an input value and stores it at its first parameter          
runOpCode3 :: Vector Int -> Int -> [Int] -> Int -> Vector Int
runOpCode3 program pointer instruction input
    | getMode 1 instruction == Position = program V.// [(program V.! (pointer + 1), input)]
    | getMode 1 instruction == Immediate = program V.// [(pointer + 1 , input)]

--Opcode 4 outputs the value at its first parameter        
runOpCode4 :: Vector Int -> Int -> [Int] -> Int        
runOpCode4 program pointer instruction
    | getMode 1 instruction == Position = (V.!) program $ program V.! (pointer+1)
    | getMode 1 instruction == Immediate = program V.! (pointer+1)
        

answerP1 :: [Int]
answerP1 = runIntCode (inputProgram V.! 1, inputProgram V.! 2) [1] inputProgram

        
--
-- Tests and input
--
testProgram :: Vector Int
testProgram = V.fromList [1002,4,3,4,33]
-- the last

