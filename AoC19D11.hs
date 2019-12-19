module AoC19D11 where

import IntCode
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M
import qualified Data.Set as S

data Direction = Up | Down | Lefty | Righty deriving (Eq, Show)

--This function returns the entire map of coordinates where the robot has been, not just the ones that were painted!
paintHull :: Vector Int -> Either String (M.Map (Int,Int) Int)
paintHull program = go (runIntCode program 0 0 []) (0,0) Up (M.empty)
    where
      go (Halt [colour, rotation]) coord _ painting = Right (M.insert coord colour painting)
      go (Continue [] f) coord direction painting = go (f 0) coord direction painting
      go (Continue [colour,rotation] f) coord direction painting = go (f $ getColour newCoord painting) newCoord newDirection (M.insert coord colour painting)
        where
          newDirection = getNewDirection rotation direction
          newCoord = getNewCoord newDirection coord
          getColour newCoord painting = M.findWithDefault 0 coord painting
      go _ _ _ _ = Left "error error"


--this should return the number of panels that have been painted at least once but the number is to low      
countPaintJobs :: Vector Int -> Either String Int
countPaintJobs program = go (runIntCode program 0 0 []) (0,0) Up (M.empty) (S.empty)
    where
      go (Halt [colour, rotation]) coord _ painting paintedPanels = Right $ S.size updatedPanels
        where
          updatedPanels = updatePanels colour coord painting paintedPanels
          
      go (Continue [] f) coord direction painting paintedPanels = go (f 0) coord direction painting paintedPanels
      
      go (Continue [colour,rotation] f) coord direction painting paintedPanels = 
            go (f $ getColour newCoord painting) newCoord newDirection (M.insert coord colour painting) updatedPanels
            
        where
          updatedPanels = updatePanels colour coord painting paintedPanels
          newDirection = getNewDirection rotation direction
          newCoord = getNewCoord newDirection coord
          getColour newCoord painting = M.findWithDefault 0 coord painting
      go _ _ _ _ _ = Left "error error"
  
updatePanels :: Int -> (Int,Int) -> M.Map (Int,Int) Int -> S.Set (Int,Int) -> S.Set (Int,Int)
updatePanels colour coord painting paintedPanels= case M.notMember coord painting of
            True -> case colour == 1 of
                        True -> S.insert coord paintedPanels
                        False -> paintedPanels
            False -> case painting M.! coord /= colour of
                        True -> S.insert coord paintedPanels
                        False -> paintedPanels 
            

getNewCoord :: Direction -> (Int,Int) -> (Int,Int)
getNewCoord Up (x,y) = (x,y+1)
getNewCoord Down (x,y) = (x,y-1)
getNewCoord Lefty (x,y) = (x-1,y)
getNewCoord Righty (x,y) = (x+1,y)            

getNewDirection :: Int -> Direction -> Direction
getNewDirection 0 Up = Lefty
getNewDirection 0 Lefty = Down
getNewDirection 0 Down = Righty
getNewDirection 0 Righty = Up
getNewDirection 1 Up = Righty
getNewDirection 1 Righty = Down
getNewDirection 1 Down = Lefty
getNewDirection 1 Lefty = Up


inputProgram :: Vector Int
inputProgram = V.fromList [3,8,1005,8,311,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,1006,0,98,2,1005,8,10,1,1107,11,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,62,1006,0,27,2,1002,12,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,90,1,1006,1,10,2,1,20,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,121,1,1003,5,10,1,1003,12,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,151,1006,0,17,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,175,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,197,2,6,14,10,1006,0,92,1006,0,4,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,229,1006,0,21,2,102,17,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,259,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,280,1006,0,58,1006,0,21,2,6,11,10,101,1,9,9,1007,9,948,10,1005,10,15,99,109,633,104,0,104,1,21101,937150919572,0,1,21102,328,1,0,1105,1,432,21101,0,387394675496,1,21102,1,339,0,1106,0,432,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,46325083283,1,1,21102,1,386,0,1106,0,432,21101,0,179519401051,1,21102,397,1,0,1106,0,432,3,10,104,0,104,0,3,10,104,0,104,0,21102,1,868410348308,1,21102,1,420,0,1105,1,432,21102,718086501140,1,1,21102,1,431,0,1105,1,432,99,109,2,22101,0,-1,1,21101,40,0,2,21101,0,463,3,21101,453,0,0,1106,0,496,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,458,459,474,4,0,1001,458,1,458,108,4,458,10,1006,10,490,1101,0,0,458,109,-2,2105,1,0,0,109,4,2102,1,-1,495,1207,-3,0,10,1006,10,513,21102,0,1,-3,22102,1,-3,1,22102,1,-2,2,21102,1,1,3,21102,1,532,0,1105,1,537,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,560,2207,-4,-2,10,1006,10,560,22101,0,-4,-4,1105,1,628,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,579,0,1105,1,537,22101,0,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,598,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,620,22102,1,-1,1,21102,1,620,0,105,1,495,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0]
