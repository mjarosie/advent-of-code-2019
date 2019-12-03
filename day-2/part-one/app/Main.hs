{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List.Split (splitOn)

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let instructionsList = map read (splitOn "," f)
    let instructionsListReplacedFirst = replaceElementAt instructionsList 1 12
    let instructionsListReplacedSecond = replaceElementAt instructionsListReplacedFirst 2 2
    print $ (runIntcodeProgram instructionsListReplacedSecond) !! 0

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
    replaceElementAt xs resIdx newVal
    where 
        newVal = (xs !! aIdx) + (xs !! bIdx)

applyMulOp :: [Int] -> Int -> Int -> Int -> [Int]
applyMulOp xs aIdx bIdx resIdx =
    replaceElementAt xs resIdx newVal
    where 
        newVal = (xs !! aIdx) * (xs !! bIdx)

replaceElementAt :: [Int] -> Int -> Int -> [Int]
replaceElementAt xs idx newVal =
    pre ++ [newVal] ++ post
    where
        pre = take idx xs
        post = drop (idx + 1) xs
