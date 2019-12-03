{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import           Data.List.Split (splitOn)

main :: IO ()
main = do
    [filename, searchValueRaw] <- Env.getArgs
    f <- readFile filename
    let instructionsList = map read (splitOn "," f)
    let searchValue = read searchValueRaw
    let result = findDesiredNounAndVerbPair instructionsList searchValue
    let noun = fst result
    let verb = snd result
    print $ 100 * noun + verb

findDesiredNounAndVerbPair :: [Int] -> Int -> (Int, Int)
findDesiredNounAndVerbPair xs searchValue = findDesiredNounAndVerbPair' xs 0 0 searchValue

findDesiredNounAndVerbPair' :: [Int] -> Int -> Int -> Int -> (Int, Int)
findDesiredNounAndVerbPair' xs noun verb searchValue
    | found                   = (noun, verb)
    | not found && verb == 99 = findDesiredNounAndVerbPair' xs (noun + 1) 0 searchValue
    | otherwise               = findDesiredNounAndVerbPair' xs noun (verb + 1) searchValue
    where
        found = (getOutputForNounAndVerb noun verb xs) == searchValue

getOutputForNounAndVerb :: Int -> Int -> [Int] -> Int
getOutputForNounAndVerb noun verb xs = 
    (runIntcodeProgram instructionsListReinitialised) !! 0
    where
        instructionsListReinitialised = replaceElementAt 2 verb . replaceElementAt 1 noun $ xs

runIntcodeProgram :: [Int] -> [Int]
runIntcodeProgram xs =
    invokeIntcodeProgramAt xs 0

invokeIntcodeProgramAt :: [Int] -> Int -> [Int]
invokeIntcodeProgramAt xs i
    | i > length xs         = xsAfterOpApplied
    | nextOperation == 99   = xsAfterOpApplied
    | otherwise             = invokeIntcodeProgramAt xsAfterOpApplied (i + 4)
    where
        nextOperation       = xs !! i
        nextOperationSet    = drop i . take (i + 4) $ xs
        xsAfterOpApplied    = invokeIntcodeOperation nextOperationSet xs

invokeIntcodeOperation :: [Int] -> [Int] -> [Int]
invokeIntcodeOperation [1, aIdx, bIdx, resIdx] xs = applyAddOp xs aIdx bIdx resIdx
invokeIntcodeOperation [2, aIdx, bIdx, resIdx] xs = applyMulOp xs aIdx bIdx resIdx
invokeIntcodeOperation (99:_) xs = xs

applyAddOp :: [Int] -> Int -> Int -> Int -> [Int]
applyAddOp xs aIdx bIdx resIdx =
    replaceElementAt resIdx newVal xs 
    where 
        newVal = (xs !! aIdx) + (xs !! bIdx)

applyMulOp :: [Int] -> Int -> Int -> Int -> [Int]
applyMulOp xs aIdx bIdx resIdx =
    replaceElementAt resIdx newVal xs 
    where 
        newVal = (xs !! aIdx) * (xs !! bIdx)

replaceElementAt :: Int -> Int -> [Int] -> [Int]
replaceElementAt idx newVal xs =
    pre ++ [newVal] ++ post
    where
        pre = take idx xs
        post = drop (idx + 1) xs
