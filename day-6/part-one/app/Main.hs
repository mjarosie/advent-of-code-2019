{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict    as Map
import qualified Data.Maybe         as Maybe

type Planet = String
type PlanetPair = (Planet, Planet)
type PlanetOrbitNumbers = Map.Map Planet ([Planet], Int) 

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let rawPlanetsOrbits = lines f
    let planetsOrbits = map (toPairTuple . splitOn ")") rawPlanetsOrbits
    let planetOrbitNumbers = getPlanetOrbitNumbers planetsOrbits
    let result = Map.foldlWithKey (\acc key value -> acc + snd value) 0 planetOrbitNumbers
    print result

toPairTuple :: [Planet] -> PlanetPair
toPairTuple [a, b] = (a, b)

getPlanetOrbitNumbers :: [PlanetPair] -> PlanetOrbitNumbers
getPlanetOrbitNumbers pp = getPlanetOrbitNumbers' pp Map.empty

getPlanetOrbitNumbers' :: [PlanetPair] -> PlanetOrbitNumbers -> PlanetOrbitNumbers
getPlanetOrbitNumbers' [] orbits = orbits
getPlanetOrbitNumbers' (pair@(inner, outer):pairs) orbits = getPlanetOrbitNumbers' pairs updatedRings
    where 
        updatedRings = insertOrbit orbits pair

insertOrbit :: PlanetOrbitNumbers -> PlanetPair -> PlanetOrbitNumbers
insertOrbit orbits pair@(inner, outer) = result
    where
        (planetsOrbittingInner, innerLevel) = Map.findWithDefault ([], 0) inner orbits
        newListOfOrbittingInner = outer:planetsOrbittingInner
        outerLevel = innerLevel + 1
        newInnerState = (newListOfOrbittingInner, innerLevel)
        orbitsWithInnerInserted = Map.insert inner newInnerState orbits
        result = updateOrbitingPlanets orbitsWithInnerInserted newListOfOrbittingInner outerLevel

updateOrbitingPlanets :: PlanetOrbitNumbers -> [Planet] -> Int -> PlanetOrbitNumbers
updateOrbitingPlanets orbits [] _ = orbits
updateOrbitingPlanets orbits (planet:planets) newLevel = updateOrbitingPlanets orbitsWithOuterUpdated planets newLevel
    where
        planetCurrentState@(outer, _) = Map.findWithDefault ([], newLevel) planet orbits
        orbitsWithInnerUpdated = Map.insert planet (outer, newLevel) orbits
        orbitsWithOuterUpdated = updateOrbitingPlanets orbitsWithInnerUpdated outer (newLevel + 1)
