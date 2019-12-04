module AoC19D4 where


-- range based on the given min and max values
inputRange :: [Int]
inputRange = [178888..676461]

inputDigits :: [[Int]]
inputDigits = fmap digits inputRange

testList :: [Int]
testList = [123456,122454,123455,122234,121000,124567,112345,112343]


-- function to create a list of all the digits in the int
-- quick and dirty but works version
digits :: Int -> [Int]
digits n = [first, second, third, fourth, fifth, sixth]
    where
      first = div n 100000
      second = mod (div n 10000) 10
      third = mod (mod (div n 1000) 100) 10
      fourth = mod (mod (mod (div n 100) 1000) 100) 10
      fifth = mod (mod (mod (mod (div n 10) 100000) 1000) 100) 10
      sixth = mod n 10 



-- function to check a list of digits for the presence of similar adjecent digits
hasSimilarAdjecent :: [Int] -> Bool
hasSimilarAdjecent [] = False
hasSimilarAdjecent (_:[]) = False
hasSimilarAdjecent (x:xs)
    | x == head xs = True
    | otherwise = hasSimilarAdjecent xs

-- checks whether the digits do not decrease    
doesNotDecrease :: [Int] -> Bool
doesNotDecrease [] = False
doesNotDecrease (_:[]) = True
doesNotDecrease (x:xs)
    | x > head xs = False
    | otherwise = doesNotDecrease xs

--check requirements
meetsRequirements :: [Int] -> Bool
meetsRequirements [] = False
meetsRequirements xs
    | hasSimilarAdjecent xs && doesNotDecrease xs = True
    | otherwise = False
    
--count number of true in a list
countTrue :: [Bool] -> Int
countTrue list = go list 0
    where
      go [] result = result
      go (x:xs) result
        | x == True = go xs (result + 1)
        | otherwise = go xs result
        
answerP1 :: Int
answerP1 = countTrue $ fmap (meetsRequirements . digits) inputRange

--check for pairs which are not part of a larger group
hasPairs :: [Int] -> Bool
hasPairs [] = False
hasPairs (_:[]) = False
hasPairs (_:_:[]) = False
hasPairs (x:y:z:zs)
    | x == y && x /= z = True
    | otherwise = go (x:y:z:zs)
        where
          go (x:y:z:[])
            | x /= y && y == z = True
            | otherwise = False
          go (x:y:z:zs)  
            | x /= y && y == z && y /= head zs = True
            | otherwise = go (y:z:zs)
    
--check requirements for second part
meetsRequirements2 :: [Int] -> Bool
meetsRequirements2 [] = False
meetsRequirements2 xs
    | hasPairs xs && doesNotDecrease xs = True
    | otherwise = False
    
answerP2 :: Int
answerP2 = countTrue $ fmap (meetsRequirements2 . digits) inputRange