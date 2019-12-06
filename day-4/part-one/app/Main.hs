{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = do
    let minPassword = 347312
    let maxPassword = 805915
    let valuesInRange = [minPassword..maxPassword]

    let valuesWithAdjacentDigits = filter hasTwoAdjacentDigits valuesInRange
    let valuesWithNoDecreasingDigits = filter (not . hasDecreasingDigits) valuesWithAdjacentDigits
    
    print $ length valuesWithNoDecreasingDigits

hasTwoAdjacentDigits :: Integer -> Bool
hasTwoAdjacentDigits x = hasTwoAdjacentDigits' (show x)

hasTwoAdjacentDigits' :: String -> Bool
hasTwoAdjacentDigits' (x1:x2:[]) = (x1 == x2)
hasTwoAdjacentDigits' (x1:x2:xs) = (x1 == x2) || hasTwoAdjacentDigits' (x2:xs)

hasDecreasingDigits :: Integer -> Bool
hasDecreasingDigits x = hasDecreasingDigits' (show x)

hasDecreasingDigits' :: String -> Bool
hasDecreasingDigits' (x1:x2:[]) = (x1 > x2)
hasDecreasingDigits' (x1:x2:xs) = (x1 > x2) || hasDecreasingDigits' (x2:xs)
