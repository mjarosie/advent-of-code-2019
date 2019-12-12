{-# LANGUAGE OverloadedStrings #-}

import qualified System.Environment as Env
import qualified Debug.Trace        as Dbg
import           Data.List.Split (splitOn)
import           Data.List (isSuffixOf)
import qualified System.IO          as SysIO

data Instruction = Instruction { operation :: Operation 
                               , parameters :: [Parameter]
                               } deriving (Show)

data ProgramContext = ProgramContext { memory :: [String]
                                     , instructionPointer :: Int
                                     } deriving (Show)

data Operation = 
      Add 
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | Halt deriving (Eq, Show, Enum)

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

runIntcodeProgram :: [String] -> IO ProgramContext
runIntcodeProgram rawInstructionsList = runIntcodeProgram' initialProgramContext
    where initialProgramContext = ProgramContext rawInstructionsList 0

runIntcodeProgram' :: ProgramContext -> IO ProgramContext
runIntcodeProgram' programContext = do
    let instruction = parseInstructionAt programContext
    if operation instruction == Halt
        then return programContext
        else do
            programContextAfterOperation <- invoke instruction programContext 
            runIntcodeProgram' programContextAfterOperation

parseInstructionAt :: ProgramContext -> Instruction
parseInstructionAt programContext@ProgramContext {memory = xs, instructionPointer = ip} =
    result
    where
        rawCommand = xs !! ip
        (op, paramsModes) = parseCommand rawCommand
        paramsCount = getNumberOfParameters op
        rawParameterValues = take paramsCount . drop (ip + 1) $ xs
        intParameterValues = map read rawParameterValues
        params = zipWith (\a b -> Parameter {value = a, mode = b}) intParameterValues paramsModes
        result = Instruction { operation = op, parameters = params}

invoke :: Instruction -> ProgramContext -> IO ProgramContext
invoke Instruction { operation = Add, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let result = show (a + b)
    let newMemoryState = replaceAt resultIdx xs result
    return ProgramContext {memory = newMemoryState, instructionPointer = ip + 4}
invoke Instruction { operation = Multiply, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let result = show (a * b)
    let newMemoryState = replaceAt resultIdx xs result
    return ProgramContext {memory = newMemoryState, instructionPointer = ip + 4}
invoke Instruction { operation = Input, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let resultIdx = value $ head params
    result <- prompt "> "
    let newMemoryState = replaceAt resultIdx xs result
    return ProgramContext {memory = newMemoryState, instructionPointer = ip + 2}
invoke Instruction { operation = Output, parameters = params } programContext@ProgramContext {memory = xs, instructionPointer = ip} = do
    print $ getParamValue xs (head params)
    return ProgramContext {memory = xs, instructionPointer = ip + 2}
invoke Instruction { operation = JumpIfTrue, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [valueToTest, ipToJump] = map (getParamValue xs) (take 2 params)
    return ProgramContext { memory = xs
                          , instructionPointer = if valueToTest /= 0 then ipToJump else ip + 3
                          }
invoke Instruction { operation = JumpIfFalse, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [valueToTest, ipToJump] = map (getParamValue xs) (take 2 params)
    return ProgramContext { memory = xs
                          , instructionPointer = if valueToTest == 0 then ipToJump else ip + 3
                          }
invoke Instruction { operation = LessThan, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let result = if a < b then "1" else "0"
    let newMemoryState = replaceAt resultIdx xs result
    return ProgramContext {memory = newMemoryState, instructionPointer = ip + 4}
invoke Instruction { operation = Equals, parameters = params } ProgramContext {memory = xs, instructionPointer = ip} = do
    let [a, b] = map (getParamValue xs) (take 2 params)
    let resultIdx = value $ last params
    let result = if a == b then "1" else "0"
    let newMemoryState = replaceAt resultIdx xs result
    return ProgramContext {memory = newMemoryState, instructionPointer = ip + 4}

replaceAt :: Int -> [a] -> a -> [a]
replaceAt idx xs val =
    let (xsBeforeIdx, restOfXs) = splitAt idx xs
        xsAfterIdx = tail restOfXs
    in xsBeforeIdx ++ [val] ++ xsAfterIdx

getParamValue :: [String] -> Parameter -> Int
getParamValue xs Parameter {value = a, mode = Immediate} = a
getParamValue xs Parameter {value = a, mode = Position} = read $ xs !! a
         
parseOperation :: String -> Operation
parseOperation xs
    | "1" `isSuffixOf` xs   = Add
    | "2" `isSuffixOf` xs   = Multiply
    | "3" `isSuffixOf` xs   = Input
    | "4" `isSuffixOf` xs   = Output
    | "5" `isSuffixOf` xs   = JumpIfTrue
    | "6" `isSuffixOf` xs   = JumpIfFalse
    | "7" `isSuffixOf` xs   = LessThan
    | "8" `isSuffixOf` xs   = Equals
    | "99" `isSuffixOf` xs  = Halt
    | otherwise             = error "Unsupported operation!"

parseCommand :: String -> (Operation, [ParameterMode])
parseCommand cmd = (op, paramsModes)
    where
        cmdLeftPadded = replicate (5 - length cmd) '0' ++ cmd
        op = parseOperation cmdLeftPadded
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
getNumberOfParameters Add           = 3
getNumberOfParameters Multiply      = 3
getNumberOfParameters Input         = 1
getNumberOfParameters Output        = 1
getNumberOfParameters Halt          = 0
getNumberOfParameters JumpIfTrue    = 2
getNumberOfParameters JumpIfFalse   = 2
getNumberOfParameters LessThan      = 3
getNumberOfParameters Equals        = 3

prompt :: String -> IO String
prompt text = do
    putStr text
    SysIO.hFlush SysIO.stdout
    getLine
