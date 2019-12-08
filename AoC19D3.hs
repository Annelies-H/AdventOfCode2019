{-# LANGUAGE TupleSections #-}
module AoC19D3 where

import Data.Set as Set

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Error (parseErrorPretty)
import System.Exit (exitFailure)

type Parser = Parsec Void String

data Direction = Up | Down | Lefty | Righty deriving (Show)


main :: IO ()
main = do
    wire1 <- parseInput parseAllDirections inputW1
    wire2 <- parseInput parseAllDirections inputW2
    let
      wire1CoordSet = Set.fromList $ allDirectionsToCoordinates (0,0) wire1
      wire2CoordSet = Set.fromList $ allDirectionsToCoordinates (0,0) wire2
      crossings = Set.intersection wire1CoordSet wire2CoordSet
      distances = Set.delete 0 $ Set.map distanceToOrigin crossings
      minDistance = Set.findMin distances
    print minDistance
    

test1W1 :: String
test1W1 = "D:/AdventOfCode2019/inputFiles/D03_test_W1.txt"
test1W2 :: String
test1W2 = "D:/AdventOfCode2019/inputFiles/D03_test_W2.txt"
test2W1 :: String
test2W1 = "D:/AdventOfCode2019/inputFiles/D03_test2_W1.txt"
test2W2 :: String
test2W2 = "D:/AdventOfCode2019/inputFiles/D03_test2_W2.txt"
inputW1 :: String
inputW1= "D:/AdventOfCode2019/inputFiles/D03W1_input.txt"
inputW2 :: String
inputW2 = "D:/AdventOfCode2019/inputFiles/D03W2_input.txt"


--
-- Part 2: fucntions to get the coordinates
--

      
--get the coordinates the wire crosses from 1 given direction and point of origin
directionToCoordinates :: (Int,Int) -> (Direction,Int) -> [(Int,Int)]
directionToCoordinates (x,y) (Up, n)     = fmap (x,) $ reverse [y+1..y+n]
directionToCoordinates (x,y) (Down, n)   = fmap (x,) [y-n..y-1] 
directionToCoordinates (x,y) (Lefty, n)  = fmap (,y) [x-n..x-1]
directionToCoordinates (x,y) (Righty, n) = fmap (,y) $ reverse [x+1..x+n] 

--get the coordinates the wire crosses based on its point of origin and all given directions plus and previous coordinates
allDirectionsToCoordinates :: (Int,Int) -> [(Direction,Int)] -> [(Int,Int)] 
allDirectionsToCoordinates origin [] = [origin]
allDirectionsToCoordinates origin directions = go origin directions [origin]
    where
      go _ [] coordinates = coordinates
      go lastCoord (x:xs) coordinates = go (head newCoords) xs (newCoords ++ coordinates)
          where
            newCoords = directionToCoordinates lastCoord x
      
--calculate the manhatten distance between two coordinates
getDistance :: (Int,Int) -> (Int,Int) -> Int
getDistance (x,y) (a,b) = (abs $ a - x) + (abs $ b - y)

--calculate the manhantten distance from the origin
distanceToOrigin :: (Int,Int) -> Int
distanceToOrigin (x,y) = abs x + abs y

--
--Part 1: get the input data 
--        
        
-- inputParser die je inputfile parsed met geselecteerde parser
-- deze is herbruikbaar voor andere puzzels of tussenparsers uitproberen ofzo
parseInput :: Parser a -> String -> IO a
parseInput parser filename = do
    input <- readFile filename
    case runParser parser filename input of 
        Left err -> putStrLn (parseErrorPretty err) >> exitFailure   -- Als er een parse error is dan prettyprint hij de error en sluit hij het programma af
        Right result -> return result --als hij successvol parsed returned hij het resultaat

-- parse all directions in a file (separated by ,) until the end of file
parseAllDirections :: Parser [(Direction, Int)]
parseAllDirections = (sepEndBy parseDirection (char ',')) <* eof

-- parsed 1 direction + afstand
parseDirection :: Parser (Direction,Int)
parseDirection = do
    direction <- lParser <|> uParser <|> dParser <|> rParser
    distance <- decimal
    return $ (direction, distance)
  where
    lParser = Lefty <$ char 'L'
    uParser = Up <$ char 'U'
    dParser = Down <$ char 'D'
    rParser = Righty <$ char 'R'
    
    



