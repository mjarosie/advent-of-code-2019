{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let masses = lines f
    let massesInt = map read masses
    
    print $ foldr (+) 0 (map calculateFuelRequirement massesInt)

calculateFuelRequirement :: Integral a => a -> a
calculateFuelRequirement mass = mass `quot` 3 - 2
