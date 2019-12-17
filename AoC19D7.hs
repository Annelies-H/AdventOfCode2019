module AoC19D7 where

import Data.List
import Data.Map as M

import AoC19D5P2
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

--puzzle 1

-- the amplifier system where the output of amplifire A goest as input to Amplifier B etc
ampA :: [Int] -> Vector Int -> [Int]
ampA phases program = runIntCode [phases !! 0, 0] program

ampB :: [Int] -> Vector Int -> [Int]
ampB phases program = runIntCode ([phases !! 1] ++ (ampA phases program)) program


ampC :: [Int] -> Vector Int -> [Int]
ampC phases program = runIntCode ([phases !! 2] ++ (ampB phases program)) program


ampD :: [Int] -> Vector Int -> [Int]
ampD phases program = runIntCode ([phases !! 3] ++ (ampC phases program)) program


ampE :: [Int] -> Vector Int -> [Int]
ampE phases program = runIntCode ([phases !! 4] ++ (ampD phases program)) program

--to find the phase combination with the highest output we need all combinations
phaseCombinationsP1 :: [[Int]]
phaseCombinationsP1 = permutations [0,1,2,3,4]

--make a map of all input combinations and the output from a list of phase combinations
thrusterPhaseMap :: [[Int]] -> Vector Int -> Map [Int] [Int]
thrusterPhaseMap phaseCombos program = go phaseCombos M.empty
    where
      go [] result = result
      go (x:xs) result = go xs (M.insert thruster x result)
          where
            thruster = ampE x program
            
--find the maximum key (thruster value) and corresponding phases
answerP1 :: Maybe ([Int],[Int])
answerP1 = M.lookupMax $ thrusterPhaseMap phaseCombinationsP1 inputProgram            

--testinput puzzle 1
test1Phases :: [Int]
test1Phases = [4,3,2,1,0]

test1Program :: Vector Int
test1Program = V.fromList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

test2Phases :: [Int]
test2Phases = [0,1,2,3,4]

test2Program :: Vector Int
test2Program = V.fromList [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]

test3Phases :: [Int]
test3Phases = [1,0,4,3,2]

test3Program :: Vector Int
test3Program = V.fromList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
