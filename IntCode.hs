module IntCode where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

data Mode = Immediate | Position | Relative deriving (Eq, Show)

data IntCodeResult
    = Continue [Int] (Int -> IntCodeResult)
    | Halt [Int]

noInput :: [Int]
noInput = []

getOutput :: Vector Int -> [Int] -> Either String [Int]
getOutput program initialInput = go (runIntCode program 0 0 []) initialInput []
    where
      go (Halt xs) _ result = Right $ result ++ xs
      go (Continue _ _) [] _ = Left "Not enough input available"
      go (Continue xs f) (y:ys) result = go (f y) ys (result ++ xs)

--initialises the IntCode and then runs the program
--opCode 99 ends the program
runIntCode :: Vector Int -> Int -> Int -> [Int] -> IntCodeResult
runIntCode program pointer base output 
          | opCode == 99 = Halt (reverse output)
            --end program
          | opCode == 1 = runIntCode (runOpCode1 (checkMemory program pointer) pointer instruction base) (pointer + 4) base output 
            -- addition
          | opCode == 2 = runIntCode (runOpCode2 (checkMemory program pointer) pointer instruction base) (pointer + 4) base output 
            -- multiplication
      --    | opCode == 3 = compute (runOpCode3 (checkMemory program pointer) pointer instruction (head input) base) (pointer + 2) base output 
          | opCode == 3 = Continue output askInputAndContinue
            -- store input
          | opCode == 4 = runIntCode program (pointer + 2)  base ((runOpCode4 (checkMemory program pointer) pointer instruction base):output)
            -- output value
          | opCode == 5 = runIntCode program (runOpCode5 (checkMemory program pointer) pointer instruction base) base output 
            -- jump if not zero
          | opCode == 6 = runIntCode program (runOpCode6 (checkMemory program pointer) pointer instruction base) base output
            -- jump if zero
          | opCode == 7 = runIntCode (runOpCode7 (checkMemory program pointer) pointer instruction base) (pointer + 4) base output 
            -- check less than
          | opCode == 8 = runIntCode (runOpCode8 (checkMemory program pointer) pointer instruction base) (pointer + 4) base output
            -- cehck equal to
          | opCode == 9 = runIntCode program (pointer + 2) (runOpCode9 (checkMemory program pointer) pointer instruction base) output 
            -- update relative base
          | otherwise = Halt []
            -- 'error'
          where
            opCode = head instruction
            instruction = reverse . getDigits $ program V.! pointer
            askInputAndContinue input = runIntCode (runOpCode3 (checkMemory program pointer) pointer instruction base input) (pointer + 2) base []

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
    | instructionDigits !! parameter == 2 = Relative
    
--function to get the values for the different parameters  that require a value
--in case the position points to a position beyond the current program the value is set at 0
getValue :: Vector Int -> Int -> [Int] -> Int -> Int -> Int
getValue program pointer instruction parameter base  =
    case getMode parameter instruction of
        Position -> case program V.! (pointer + parameter) < V.length program of
                                    True -> (V.!) program $ program V.! (pointer + parameter)
                                    False -> 0
        Immediate -> program V.! (pointer + parameter)
        Relative -> case (base + program V.! (pointer + parameter)) < V.length program of
                                    True -> (V.!) program $ base + program V.! (pointer + parameter)
                                    False -> 0

--function to get the position from a parameter to which the respective opCode outputs
--in case the position points to a position beyond the current program the returnded position is set at 0      
getPosition :: Vector Int -> Int -> [Int] -> Int -> Int -> Int
getPosition program pointer instruction parameter base =
    case getMode parameter instruction of
        Position -> case (pointer + parameter) < V.length program of
                        True -> program V.! (pointer + parameter)
                        False -> 0
        Immediate -> pointer + parameter
        Relative -> case program V.! (pointer + parameter) < V.length program of
                        True -> base + program V.! (pointer + parameter)
                        False -> 0
        
--function to adjust the memory if the position written to is larger then the maximum position
--checkMemory is run before running an opCode and in the different opCodes before writing a value to a position
checkMemory :: Vector Int -> Int -> Vector Int
checkMemory program position
    | position < V.length program = program
    | otherwise = (V.++) program $ V.replicate (1 + position - V.length program) 0

--Opcode 1 takes the values of its firs two paramaters
--adds them and stores them at its third paramater
runOpCode1 :: Vector Int -> Int -> [Int] -> Int -> Vector Int
runOpCode1 program pointer instruction base = (checkMemory program param3) V.// [(param3 , param1 + param2)]
    where
        param1 = getValue program pointer instruction 1 base
        param2 = getValue program pointer instruction 2  base
        param3 = getPosition program pointer instruction 3 base

--Opcode 2 takes the values of its first two parameters
--mulitplies them and stores them at its third paramater
runOpCode2 :: Vector Int -> Int -> [Int] -> Int -> Vector Int
runOpCode2 program pointer instruction base = (checkMemory program param3) V.// [(param3 ,param1 * param2)]
    where
        param1 = getValue program pointer instruction 1 base
        param2 = getValue program pointer instruction 2 base
        param3 = getPosition program pointer instruction 3 base  

--Opcode 3 takes an input value and stores it at its first parameter          
runOpCode3 :: Vector Int -> Int -> [Int] -> Int -> Int -> Vector Int
runOpCode3 program pointer instruction base input = (checkMemory program param1) V.// [(param1, input)]
    where
        param1 = getPosition program pointer instruction 1 base

--Opcode 4 outputs the value at its first parameter        
runOpCode4 :: Vector Int -> Int -> [Int] -> Int -> Int        
runOpCode4 program pointer instruction base = getValue program pointer instruction 1 base

--Opcode 5 jumps the pointer to its second paramater if the first is non-zero
runOpCode5 :: Vector Int -> Int -> [Int] -> Int -> Int
runOpCode5 program pointer instruction base 
    | param1 == 0 = pointer + 3
    | otherwise = param2
        where
            param1 = getValue program pointer instruction 1 base
            param2 = getValue program pointer instruction 2 base

--Opcode 6 jumps the pointer to its second paramater if the first is zero
runOpCode6 :: Vector Int -> Int -> [Int] -> Int -> Int
runOpCode6 program pointer instruction base 
    | param1 /= 0 = pointer + 3
    | otherwise = param2
        where
            param1 = getValue program pointer instruction 1 base
            param2 = getValue program pointer instruction 2 base
                
--OpCode 7 checks of its first parameter is less than it second paramater
--stores a 1 in its third parameter if true or a 0 if false
runOpCode7 :: Vector Int -> Int -> [Int] -> Int -> Vector Int
runOpCode7 program pointer instruction base
    | param1 < param2 = (checkMemory program param3) V.// [(param3,1)]
    | otherwise = (checkMemory program param3) V.//[(param3,0)]
        where
          param1 = getValue program pointer instruction 1 base
          param2 = getValue program pointer instruction 2 base
          param3 = getPosition program pointer instruction 3 base

--OpCode 7 checks of its first parameter equal to its second paramater
--stores a 1 in its third parameter if true or a 0 if false
runOpCode8 :: Vector Int -> Int -> [Int] -> Int -> Vector Int
runOpCode8 program pointer instruction base
    | param1 == param2 = (checkMemory program param3) V.// [(param3,1)]
    | otherwise = (checkMemory program param3) V.//[(param3,0)]
        where
          param1 = getValue program pointer instruction 1 base
          param2 = getValue program pointer instruction 2 base
          param3 = getPosition program pointer instruction 3 base

--OpCode 9 changes the relative base by adding its only paramter
runOpCode9 :: Vector Int -> Int -> [Int] -> Int -> Int
runOpCode9 program pointer instruction base = base + param1
    where
        param1 = getValue program pointer instruction 1 base
          
          
--
-- Tests and input
--
runTest :: Vector Int -> [Int]
runTest test = go (runIntCode test 0 0 [])
    where
      go (Halt result) = result
      go _ = []

test1 :: Vector Int
test1 = V.fromList [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
-- test 1 should output a copy of itself

test2 :: Vector Int
test2 = V.fromList [1102,34915192,34915192,7,4,7,99,0]
-- test 2 should output a 16 digit number

test3 :: Vector Int
test3 = V.fromList [104,1125899906842624,99]
-- test 3 should output the large number in the middle

