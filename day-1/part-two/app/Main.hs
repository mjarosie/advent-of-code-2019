{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let numericLines = map read (lines f)
    print $ getFuelRequirementForAllModules numericLines

getFuelRequirementForAllModules :: Integral a => [a] -> a
getFuelRequirementForAllModules xs = foldr (+) 0 (map calculateFuelRequirementForModule xs)

calculateFuelRequirementForModule :: Integral a => a -> a
calculateFuelRequirementForModule x
    | fuelRequired > 0  = fuelRequired + calculateFuelRequirementForModule fuelRequired
    | otherwise = 0
    where 
        fuelRequired = x `quot` 3 - 2
