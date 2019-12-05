{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified System.Environment as Env

import           Data.List.Split (splitOn)

type Coordinate = (Integer, Integer)
type Direction = (Char, Integer)

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let rawWiresDirections = lines f
    let rawWiresDirectionsSplit = map (splitOn ",") rawWiresDirections
    let wiresDirections = map extractDirections rawWiresDirectionsSplit
    let minimumSumOfLenghtsUntilIntersection = 
            getMinimumCombinedLenghtOfWiresUntilFirstIntersection wiresDirections
    print minimumSumOfLenghtsUntilIntersection

extractDirections :: [String] -> [Direction]
extractDirections [] = []
extractDirections (x:xs) = (head x, read (tail x)):extractDirections xs

getMinimumCombinedLenghtOfWiresUntilFirstIntersection :: [[Direction]] -> Int
getMinimumCombinedLenghtOfWiresUntilFirstIntersection xs =
     minimum sumOfLengthsUntilIntersection
     where
        sumOfLengthsUntilIntersection = map (foldr (+) 0) lengthsUntilIntersectionInWires
        lengthsUntilIntersectionInWires = map (map (+1)) indicesOfIntersectionsInWires
        indicesOfIntersectionsInWires = findElementsInLists commonElements wirePathes
        commonElements = getCommonElements wirePathes
        wirePathes = map getDirectionPath xs

getDirectionPath :: [Direction] -> [Coordinate]
getDirectionPath dirs = tail (getDirectionPath' dirs [(0, 0)])

getDirectionPath' :: [Direction] -> [Coordinate] -> [Coordinate]
getDirectionPath' [] coords = coords
getDirectionPath' (dir:dirs) coords = getDirectionPath' dirs (coords ++ newCoords)
    where newCoords = getDirectionCoordinates dir (last coords)

findElementsInLists :: Eq a => [a] -> [[a]] -> [[Int]]
findElementsInLists [el] xs = [findElementInLists el xs]
findElementsInLists (el:els) xs = (findElementInLists el xs):(findElementsInLists els xs)

findElementInLists :: Eq a => a -> [[a]] -> [Int]
findElementInLists el xs = map (findElementInList el) xs

findElementInList :: Eq a => a -> [a] -> Int
findElementInList el xs = Maybe.fromJust (List.findIndex ((==) el) xs)

getCommonElements :: [[Coordinate]] -> [Coordinate]
getCommonElements xs = Set.toList (getCommonElements' xs)

getCommonElements' :: [[Coordinate]] -> Set.Set Coordinate
getCommonElements' [x] = Set.fromList x
getCommonElements' (x:xs) = Set.intersection (Set.fromList x) (getCommonElements' xs)

getMinCoordinate :: [Coordinate] -> Coordinate
getMinCoordinate (x:xs) = getMinCoordinate' x xs

getMinCoordinate' :: Coordinate -> [Coordinate] -> Coordinate
getMinCoordinate' m [] = m
getMinCoordinate' m (x:xs)
    | manhattanDistance x < manhattanDistance m   = getMinCoordinate' x xs
    | otherwise                 = getMinCoordinate' m xs

manhattanDistance :: Coordinate -> Integer
manhattanDistance (x, y) = abs x + abs y

getDirectionCoordinates :: Direction -> Coordinate -> [Coordinate]
getDirectionCoordinates ('R', l) coord = map (addToFirstCoordinate coord) [1..l]
getDirectionCoordinates ('L', l) coord = map (subtractFromFirstCoordinate coord) [1..l]
getDirectionCoordinates ('U', l) coord = map (addToSecondCoordinate coord) [1..l]
getDirectionCoordinates ('D', l) coord = map (subtractFromSecondCoordinate coord) [1..l]

addToFirstCoordinate :: Coordinate -> Integer -> Coordinate
addToFirstCoordinate (a, b) k = (a + k, b)

addToSecondCoordinate :: Coordinate -> Integer -> Coordinate
addToSecondCoordinate (a, b) k = (a , b + k)

subtractFromFirstCoordinate :: Coordinate -> Integer -> Coordinate
subtractFromFirstCoordinate (a, b) k = (a - k, b)

subtractFromSecondCoordinate :: Coordinate -> Integer -> Coordinate
subtractFromSecondCoordinate (a, b) k = (a , b - k)
