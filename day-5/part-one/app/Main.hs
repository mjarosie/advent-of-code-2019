{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List.Split (splitOn)
import           Data.List (isSuffixOf)
import qualified System.IO          as SysIO

data Instruction = Instruction { operation :: Operation 
                               , parameters :: [Parameter]
                               } deriving (Show)
data Operation = Add | Multiply | Input | Output | Halt deriving (Eq, Show, Enum)
data Parameter = Parameter { value :: Int
                           , mode :: ParameterMode
                           } deriving (Show)
data ParameterMode = Position | Immediate deriving (Eq, Show, Enum)

main :: IO ()
main = do
    [filename] <- Env.getArgs
    f <- readFile filename
    let rawInstructionsList = splitOn "," f
    result <- runIntcodeProgram rawInstructionsList
    putStrLn "Intcode program finished!"

runIntcodeProgram :: [String] -> IO [String]
runIntcodeProgram rawInstructionsList = runIntcodeProgram' rawInstructionsList 0
    
runIntcodeProgram' :: [String] -> Int -> IO [String]
runIntcodeProgram' programMemory pc = do
    let instruction = parseInstructionAt programMemory pc
    if operation instruction == Halt
        then return programMemory
        else do
            let numberOfParameters = getNumberOfParameters (operation instruction)
            memoryAfterOperation <- invoke instruction programMemory
            runIntcodeProgram' memoryAfterOperation (pc + 1 + numberOfParameters)

parseInstructionAt :: [String] -> Int -> Instruction
parseInstructionAt xs pc =
    result
    where
        rawCommand = xs !! pc
        (op, paramsModes) = parseCommand rawCommand
        paramsCount = getNumberOfParameters op
        rawParameterValues = take paramsCount . drop (pc + 1) $ xs
        intParameterValues = map read rawParameterValues
        params = zipWith (\a b -> Parameter {value = a, mode = b}) intParameterValues paramsModes
        result = Instruction { operation = op, parameters = params}

invoke :: Instruction -> [String] -> IO [String]
invoke Instruction { operation = Add, parameters = params } xs = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let (xsBeforeResultIndex, restOfXs) = splitAt resultIdx xs
    let xsAfterResultIndex = tail restOfXs
    let result = show (a + b)
    return $ xsBeforeResultIndex ++ [result] ++ xsAfterResultIndex
invoke Instruction { operation = Multiply, parameters = params } xs = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let (xsBeforeResultIndex, restOfXs) = splitAt resultIdx xs
    let xsAfterResultIndex = tail restOfXs
    let result = show (a * b)
    return $ xsBeforeResultIndex ++ [result] ++ xsAfterResultIndex
invoke Instruction { operation = Input, parameters = params } xs = do
    let resultIdx = value $ head params
    let (xsBeforeResultIndex, restOfXs) = splitAt resultIdx xs
    let xsAfterResultIndex = tail restOfXs
    result <- prompt "> "
    return $ xsBeforeResultIndex ++ [result] ++ xsAfterResultIndex
invoke Instruction { operation = Output, parameters = params } xs = do
    print $ getParamValue xs (head params)
    return xs

getParamValue :: [String] -> Parameter -> Int
getParamValue xs Parameter {value = a, mode = Immediate} = a
getParamValue xs Parameter {value = a, mode = Position} = read $ xs !! a
         
parseOperation :: String -> Operation
parseOperation xs
    | "1" `isSuffixOf` xs   = Add
    | "2" `isSuffixOf` xs   = Multiply
    | "3" `isSuffixOf` xs   = Input
    | "4" `isSuffixOf` xs   = Output
    | "99" `isSuffixOf` xs  = Halt
    | otherwise             = error "Unsupported operation!"

parseCommand :: String -> (Operation, [ParameterMode])
parseCommand cmd = (op, paramsModes)
    where
        cmdLeftPadded = replicate (5 - length cmd) '0' ++ cmd
        op = parseOperation cmdLeftPadded
        numberOfParameters = getNumberOfParameters op
        paramsModes = getParameterModes cmdLeftPadded

parseParameters :: [String] -> [Parameter]
parseParameters xs =
    zipWith (\a b -> Parameter { value = a, mode = b}) values operationModes 
    where
        rawOperationDescriptor = head xs
        op = parseOperation rawOperationDescriptor
        numberOfParameters = getNumberOfParameters op
        operationModes = take numberOfParameters (getParameterModes rawOperationDescriptor)
        values = map read (take numberOfParameters (tail xs))

getParameterModes :: String -> [ParameterMode]
getParameterModes paddedCmd
    | "99" `isSuffixOf` paddedCmd   = []
    | otherwise                     = modifiersParsed
    where 
        modeDescriptors = take 3 paddedCmd
        modifiersParsed = reverse $ map parseParameterMode modeDescriptors

parseParameterMode :: Char -> ParameterMode
parseParameterMode c
    | c == '0'  = Position
    | c == '1'  = Immediate
    | otherwise = error "Unsupported parameter mode!"

getNumberOfParameters :: Operation -> Int
getNumberOfParameters Add = 3
getNumberOfParameters Multiply  = 3
getNumberOfParameters Input     = 1
getNumberOfParameters Output    = 1
getNumberOfParameters Halt      = 0

prompt :: String -> IO String
prompt text = do
    putStr text
    SysIO.hFlush SysIO.stdout
    getLine
