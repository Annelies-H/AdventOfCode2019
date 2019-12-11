module AoC19D5P2 where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data Mode = Immediate | Position deriving (Eq, Show)

--initialises the IntCode and then runs the program
--opCode 99 ends the program
runIntCode :: [Int] -> Vector Int -> [Int]
runIntCode input program = compute program 0 input []
    where
      compute program pointer input output
          | opCode == 99 = output
          | opCode == 1 = compute (runOpCode1 program pointer instruction) (pointer + 4) input output
          | opCode == 2 = compute (runOpCode2 program pointer instruction) (pointer + 4) input output
          | opCode == 3 = compute (runOpCode3 program pointer instruction (head input)) (pointer + 2) (tail input) output
          | opCode == 4 = compute program (pointer + 2) input ((runOpCode4 program pointer instruction):output)
          | opCode == 5 = compute program (runOpCode5 program pointer instruction) input output
          | opCode == 6 = compute program (runOpCode6 program pointer instruction) input output
          | opCode == 7 = compute (runOpCode7 program pointer instruction) (pointer + 4) input output
          | opCode == 8 = compute (runOpCode8 program pointer instruction) (pointer + 4) input output
          | otherwise = []
          where
            opCode = head instruction
            instruction = reverse . getDigits $ program V.! pointer

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
          | getMode 3 instruction == Immediate = pointer+3

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
          | getMode 3 instruction == Immediate = pointer+3       

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

--Opcode 5 jumps the pointer to its second paramater if the first is non-zero
runOpCode5 :: Vector Int -> Int -> [Int] -> Int
runOpCode5 program pointer instruction
    | param1 == 0 = pointer + 3
    | otherwise = param2
        where
            param1 = case getMode 1 instruction of
                Position -> (V.!) program $ program V.! (pointer+1)
                Immediate -> program V.! (pointer+1)
            param2 = case getMode 2 instruction of
                Position -> (V.!) program $ program V.! (pointer+2)
                Immediate -> program V.! (pointer+2)

--Opcode 6 jumps the pointer to its second paramater if the first is zero
runOpCode6 :: Vector Int -> Int -> [Int] -> Int
runOpCode6 program pointer instruction
    | param1 /= 0 = pointer + 3
    | otherwise = param2
        where
            param1 = case getMode 1 instruction of
                Position -> (V.!) program $ program V.! (pointer+1)
                Immediate -> program V.! (pointer+1)
            param2 = case getMode 2 instruction of
                Position -> (V.!) program $ program V.! (pointer+2)
                Immediate -> program V.! (pointer+2)
                
--OpCode 7 checks of its first parameter is less than it second paramater
--stores a 1 in its third parameter if true or a 0 if false
runOpCode7 :: Vector Int -> Int -> [Int] -> Vector Int
runOpCode7 program pointer instruction
    | param1 < param2 = program V.// [(param3,1)]
    | otherwise = program V.//[(param3,0)]
        where
          param1 = case getMode 1 instruction of
              Position -> (V.!) program $ program V.! (pointer+1)
              Immediate -> program V.! (pointer+1)
          param2 = case getMode 2 instruction of
              Position -> (V.!) program $ program V.! (pointer+2)
              Immediate -> program V.! (pointer+2)
          param3 = case getMode 3 instruction of
              Position -> program V.! (pointer+3)
              Immediate -> pointer+3

--OpCode 7 checks of its first parameter equal to its second paramater
--stores a 1 in its third parameter if true or a 0 if false
runOpCode8 :: Vector Int -> Int -> [Int] -> Vector Int
runOpCode8 program pointer instruction
    | param1 == param2 = program V.// [(param3,1)]
    | otherwise = program V.//[(param3,0)]
        where
          param1 = case getMode 1 instruction of
              Position -> (V.!) program $ program V.! (pointer+1)
              Immediate -> program V.! (pointer+1)
          param2 = case getMode 2 instruction of
              Position -> (V.!) program $ program V.! (pointer+2)
              Immediate -> program V.! (pointer+2)
          param3 = case getMode 3 instruction of
              Position -> program V.! (pointer+3)
              Immediate -> pointer+3
--
-- Tests and input
--
test1 :: Vector Int
test1 = V.fromList [3,9,8,9,10,9,4,9,99,-1,8]

test2 :: Vector Int
test2 = V.fromList [3,9,7,9,10,9,4,9,99,-1,8]

test3 :: Vector Int
test3 = V.fromList [3,3,1108,-1,8,3,4,3,99]

test4 :: Vector Int
test4 = V.fromList [3,3,1107,-1,8,3,4,3,99]



test5 :: Vector Int
test5 = V.fromList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]

test6 :: Vector Int
test6 = V.fromList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]

test7 :: Vector Int
test7 = V.fromList [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
