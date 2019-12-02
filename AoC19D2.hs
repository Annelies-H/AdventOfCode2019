module AoC19D2 where


-- function to change the nth element of a list into a given value
-- returns an empty list if the index does not exist
changeElement :: (Eq a) => Int -> a -> [a] -> [a] 
changeElement index value list
    | list == [] = []
    | index < 0 = []
    | index > length list = []
    | otherwise = take index list ++ [value] ++ drop (index + 1) list


-- compute the new list based on the opcode at postion
compute :: Int -> [Int] -> [Int]
compute opCodePosition list
    | list !! opCodePosition == 1 = changeElement n x list
    | list !! opCodePosition == 2 = changeElement n y list
    | list !! opCodePosition == 99 = list
    | otherwise = []
        where
          n = (!!) list $ opCodePosition + 3
          x = a + b
          y = a * b
          a = (!!) list $ (!!) list $ opCodePosition + 1
          b = (!!) list $ (!!) list $ opCodePosition + 2
          
-- run the computations over the program list
intCodeProgram :: Int -> [Int] -> [Int]
intCodeProgram _ [] = []
intCodeProgram pointer list
    | list!! pointer == 99 = list
    | otherwise = intCodeProgram (pointer+4) newList
        where
          newList = compute pointer list

-- answer to question 1, what is the first value in the list if
-- the porgram is initialised with 12 at postion 1 and 2 at postion 2

answerP1 :: Int
answerP1 = output (12,2) inputList

--function to create the output to the program initialised with a noun and verb
--output is the first element in the list after the program is run
output :: (Int,Int) -> [Int] -> Int
output nounverb program = 
    head $ intCodeProgram 0 $ initialiseProgram nounverb program

-- function to initialise the program with a given noun and verb
initialiseProgram :: (Int,Int) -> [Int] -> [Int]
initialiseProgram (noun,verb) program =
    changeElement posNoun noun $ changeElement posVerb verb program

-- positions of the noun and the verb in the program 
-- where the first element has position 0
posNoun :: Int    
posNoun = 1
posVerb :: Int
posVerb = 2

-- list to provide all possible cominations of two lists
permutations :: [a] -> [(a,a)]
permutations [] = []
permutations (x:xs) = go x xs (x:xs) []
    where
      go _ [] _ result = result
      go _ _ [] result = result
      go x (y:ys) originalList result = go y ys originalList (result ++ fmap ((,)x) originalList)


--find the noun and verb resulting in the correct output
findNounVerb :: [(Int,Int)] -> [Int] -> (Int,Int)
findNounVerb _ [] = (-9999,0)
findNounVerb [] program = (0,-9999)
findNounVerb ((noun,verb):xs) program
    | output (noun,verb) program == 19690720 = (noun,verb)
    | otherwise = findNounVerb xs program

--create a list of inputtuples with all combinations of nouns and verbs between 1 and 99    
inputNounVerbs :: [(Int,Int)]
inputNounVerbs = permutations [1..99]

answerCalculation :: (Int,Int) -> Int
answerCalculation (noun,verb) = 100 * noun + verb

answerP2 :: Int
answerP2 = answerCalculation $ findNounVerb inputNounVerbs inputList


--
-- Tests and input
--
testList :: [Int]
testList = [1,1,1,4,99,5,6,0,99]

testList2 :: [Int]
testList2 = [2,4,4,5,99,0]

testList3 :: [Int]
testList3 = [1,0,0,0,99]

testList4 :: [Int]
testList4 = [1,9,10,3,2,3,11,0,99,30,40,50]

inputList :: [Int]
inputList = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23
    ,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1
    ,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10
    ,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13
    ,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123
    ,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0
    ,14,0]