{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict    as Map
import qualified Data.Maybe         as Maybe

type Planet = String
type PlanetPair = (Planet, Planet)
type PlanetOrbitNumbers = Map.Map Planet
    (
      [Planet]      -- planets orbiting the key (children)
    , Maybe Planet  -- planet that the key is orbiting (parent)
    , Int           -- level of the orbit
    )

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let rawPlanetsOrbits = lines f
    let planetsOrbits = map (toPairTuple . splitOn ")") rawPlanetsOrbits
    let planetOrbitNumbers = getPlanetOrbitNumbers planetsOrbits
    let result = findNumberOfOrbitalTransfers "YOU" "SAN" planetOrbitNumbers
    print result

toPairTuple :: [Planet] -> PlanetPair
toPairTuple [a, b] = (a, b)

getPlanetOrbitNumbers :: [PlanetPair] -> PlanetOrbitNumbers
getPlanetOrbitNumbers pp = getPlanetOrbitNumbers' pp Map.empty

getPlanetOrbitNumbers' :: [PlanetPair] -> PlanetOrbitNumbers -> PlanetOrbitNumbers
getPlanetOrbitNumbers' [] orbits = orbits
getPlanetOrbitNumbers' (pair@(inner, outer):pairs) orbits = getPlanetOrbitNumbers' pairs updatedOrbits
    where
        updatedOrbits = insertOrbit orbits pair

insertOrbit :: PlanetOrbitNumbers -> PlanetPair -> PlanetOrbitNumbers
insertOrbit orbits pair@(inner, outer) = result
    where
        (planetsOrbittingInner, planetThatInnerOrbits, innerLevel) = Map.findWithDefault ([], Nothing, 0) inner orbits
        newListOfOrbittingInner = outer:planetsOrbittingInner
        outerLevel = innerLevel + 1
        newInnerState = (newListOfOrbittingInner, planetThatInnerOrbits, innerLevel)
        orbitsWithInnerInserted = Map.insert inner newInnerState orbits
        result = updateOrbitingPlanets orbitsWithInnerInserted newListOfOrbittingInner inner outerLevel

updateOrbitingPlanets :: PlanetOrbitNumbers -> [Planet] -> Planet -> Int -> PlanetOrbitNumbers
updateOrbitingPlanets orbits [] _ _ = orbits
updateOrbitingPlanets orbits (planet:planets) parentPlanet newLevel = updateOrbitingPlanets orbitsWithOuterUpdated planets parentPlanet newLevel
    where
        (outerPlanets, _, _) = Map.findWithDefault ([], Nothing, newLevel) planet orbits
        orbitsWithInnerUpdated = Map.insert planet (outerPlanets, Just parentPlanet, newLevel) orbits
        orbitsWithOuterUpdated = updateOrbitingPlanets orbitsWithInnerUpdated outerPlanets planet (newLevel + 1)

getOrbitsChainFrom :: PlanetOrbitNumbers -> Planet -> [Planet]
getOrbitsChainFrom orbits planet = getOrbitsChainFrom' orbits (Just planet)

getOrbitsChainFrom' :: PlanetOrbitNumbers -> Maybe Planet -> [Planet]
getOrbitsChainFrom' _ Nothing = []
getOrbitsChainFrom' orbits (Just planet) = planet:getOrbitsChainFrom' orbits maybeParent
    where
        (_, maybeParent, _) = orbits Map.! planet

findFirstCommonElement :: [Planet] -> [Planet] -> Planet
findFirstCommonElement (planet:planets) otherPlanets
    | planet `elem` otherPlanets    = planet
    | otherwise                     = findFirstCommonElement planets otherPlanets

findNumberOfOrbitalTransfers :: Planet -> Planet -> PlanetOrbitNumbers -> Int
findNumberOfOrbitalTransfers first second orbits = (firstPlanetLevel - 1 - commonPlanetLevel) + (secondPlanetLevel - 1 - commonPlanetLevel)
    where
        firstChain = getOrbitsChainFrom orbits first
        secondChain = getOrbitsChainFrom orbits second
        firstCommon = findFirstCommonElement (tail firstChain) (tail secondChain)
        (_, _, firstPlanetLevel) = orbits Map.! first
        (_, _, secondPlanetLevel) = orbits Map.! second
        (_, _, commonPlanetLevel) = orbits Map.! firstCommon
