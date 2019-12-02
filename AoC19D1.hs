module AoC19D1 where

import System.Environment

{-
https://adventofcode.com/2019/day/1
-}

-- specify inputfile with all module weights
inputfile = "D:/AdventOfCode2019/D01P1_input.txt"

-- testlist
testList :: [Int]
testList = [90, 3000, 200]

-- calculate fuel from mass and sum over the list
sumFuel :: [String] -> Int
sumFuel = sum . fmap (\x -> read x `div` 3 - 2)

-- read input file and apply function
answerP1 :: IO Int
answerP1 = sumFuel . lines <$> readFile inputfile


-- calculate the fuel based on the mass, below mass 6 no fuel is required
massToFuel :: Int -> Int
massToFuel mass
    | mass < 6 = 0
    | otherwise = div mass 3 - 2

-- recursive function to calculate the fuel needed by a moduel
-- taking into account the fuel needed to carry the fuel   
moduleToFuel :: Int -> Int
moduleToFuel moduleMass = go moduleMass 0
    where
        go 0 result = result
        go mass result = go value (result + value)
            where
                value = massToFuel mass

-- read the file and convert it to a list of strings using lines
-- fmap to convert string to int and calcuelate the fuel needed for the module
-- sum the reulsts
answerP2 :: IO Int
answerP2 = sum . fmap (moduleToFuel.read) . lines <$> readFile inputfile



main :: IO ()
main = do
	print "Advent Of Code"