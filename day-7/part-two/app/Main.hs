{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List (permutations)
import           System.Process (
                                  readCreateProcess
                                , createProcess
                                , proc
                                , cwd
                                )

main :: IO ()
main = do
    [filename] <- Env.getArgs
    results <- mapM (getThrustersOutputSignal filename 0) (permutations [0, 1, 2, 3, 4])
    let siema = maximum results
    print siema
    -- r <- readCreateProcess (proc "stack" ["exec", "run-intcode-program", "../../day-7/input"]) { cwd = Just "../../day-5/part-two/" } "0\r\n0\r\n"
    -- result <- getThrustersOutputSignal filename 0 [0, 1, 2, 3, 4] 
    -- print $ result 

getThrustersOutputSignal :: String -> Int -> [Int] -> IO Int
getThrustersOutputSignal _ input [] = return input
getThrustersOutputSignal filename input (phase:phases) =
    do
        response <- executeIntcodeProgram filename phase input
        getThrustersOutputSignal filename response phases

executeIntcodeProgram :: String -> Int -> Int -> IO Int
executeIntcodeProgram filename phase input = do 
    let rawInput = show phase ++ "\r\n" ++ show input ++ "\r\n"
    rawResponse <- readCreateProcess (proc "stack" ["exec", "run-intcode-program", "../../day-7/" ++ filename]) { cwd = Just "../../day-5/part-two/" } rawInput
    return $ read rawResponse
