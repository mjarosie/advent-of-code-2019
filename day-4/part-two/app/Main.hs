{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = do
    let minPassword = 347312
    let maxPassword = 805915
    let valuesInRange = [minPassword..maxPassword]

    let valuesWithExactlyTwoAdjacentDigits = filter hasExactlyTwoAdjacentDigits valuesInRange
    let valuesWithNoDecreasingDigits = filter (not . hasDecreasingDigits) valuesWithExactlyTwoAdjacentDigits
    
    print $ length valuesWithNoDecreasingDigits

hasExactlyTwoAdjacentDigits :: Integer -> Bool
-- Handling the edge case in which the first two digits are matching
-- Other cases are handled in hasExactlyTwoAdjacentDigits'
hasExactlyTwoAdjacentDigits x = (x1 == x2) && (x2 /= x3) || hasExactlyTwoAdjacentDigits' (show x)
    where (x1:x2:x3:xs) = show x

hasExactlyTwoAdjacentDigits' :: String -> Bool
hasExactlyTwoAdjacentDigits' (x1:x2:x3:[]) = (x1 /= x2) && (x2 == x3)
hasExactlyTwoAdjacentDigits' (x1:x2:x3:x4:xs)
    | (x1 /= x2) && (x2 == x3) && (x3 /= x4)    = True
    | otherwise                                 = restOfAdjacentDigits
    where restOfAdjacentDigits = hasExactlyTwoAdjacentDigits' (x2:x3:x4:xs)

hasDecreasingDigits :: Integer -> Bool
hasDecreasingDigits x = hasDecreasingDigits' (show x)

hasDecreasingDigits' :: String -> Bool
hasDecreasingDigits' (x1:x2:[]) = (x1 > x2)
hasDecreasingDigits' (x1:x2:xs) = (x1 > x2) || hasDecreasingDigits' (x2:xs)
