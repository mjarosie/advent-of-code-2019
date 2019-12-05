{-# LANGUAGE OverloadedStrings #-}

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
    print $ getClosestIntersection wiresDirections

extractDirections :: [String] -> [Direction]
extractDirections [] = []
extractDirections (x:xs) = (head x, read (tail x)):extractDirections xs

getClosestIntersection :: [[Direction]] -> Integer
getClosestIntersection xs = manhattanDistance closestIntersection
    where
        closestIntersection = getMinCoordinate commonElements
        commonElements = getCommonElements wirePathes
        wirePathes = map getDirectionPath xs

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

getDirectionPath :: [Direction] -> [Coordinate]
getDirectionPath dirs = tail (getDirectionPath' dirs [(0, 0)])

getDirectionPath' :: [Direction] -> [Coordinate] -> [Coordinate]
getDirectionPath' [] coords = coords
getDirectionPath' (dir:dirs) coords = getDirectionPath' dirs (coords ++ newCoords)
    where newCoords = getDirectionCoordinates dir (last coords)

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
