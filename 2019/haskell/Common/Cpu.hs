module Common.Cpu
( execute
, executeFor
, executeNoInput
, executeFromState
, primeTape
, newTape
, newState
, parseInput
, execState
, tapeState
, outputState
, ipState
, inputState
, mem
, testProgram

, Tape
, ExecState(..)
, State
, Result

, cpuTests
)
where

import Data.Foldable
import Data.Either
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception

import Common.Util (assertEqual, wordsWhen, ioTrace, ioTraceDirect)


data Tape = Tape { mem        :: Seq Int 
                 , extMem     :: Map Int Int
                 , capacity   :: Int
                 , baseOffset :: Int
                 }
                 deriving (Eq, Show)
data ExecState = Running | Halt | Suspended 
                 deriving (Eq, Show)
data State = State { execState    :: ExecState
                   , tapeState    :: Tape
                   , outputState  :: [Int]
                   , ipState      :: Int
                   , inputState   :: [Int] 
                   }
                   deriving (Eq, Show)
type Result = Either String State
data Param = Positional Int 
           | Immediate Int
           | Relative Int
           deriving Show

type OpResult = (Tape, [Int], Maybe Int) -- (tape, outputs, ?new-ip)

enableTrace = False :: Bool
maxCycles = 10000000 :: Int


execute :: Tape -> [Int] -> Result
execute tape input = executeFor tape input maxCycles

executeFor :: Tape -> [Int] -> Int -> Result
executeFor tape input cpuCycles = step (ok tape [] 0 input) cpuCycles

executeNoInput :: Tape -> Result
executeNoInput tape = execute tape []

executeFromState :: State -> Result
executeFromState state = step (Right state) maxCycles

step :: Result -> Int -> Result
step initialState cpuTime = result
  where 
    result = case initialState of 
      Left e -> err e
      Right state -> cycleResult
        where 
          tape = tapeState (traceable state)
          output = outputState state
          ip = ipState state
          input = inputState state
          op 
            | cpuTime > 0 = getOp $ getOne tape ip
            | otherwise   = -1
          
          proceedWithAuxInput = \f narg aux consumedInputs -> 
            let (opTape, opOutput, opIp) = f tape (getParams narg tape ip aux)
            in step (ok opTape (output ++ opOutput) 
                    (advance ip (narg+1) opIp)
                    (drop consumedInputs input)) (cpuTime-1) 

          proceedWithAuxInputOrSuspend = \f narg aux consumedInputs ->
            if (consumedInputs <= (length input)) 
                then proceedWithAuxInput f narg aux consumedInputs
                else suspend tape output ip input
          
          proceed f narg = proceedWithAuxInput f narg [] 0

          cycleResult = 
            case op of
              1 -> proceed opAdd 3
              2 -> proceed opMult 3 
              3 -> proceedWithAuxInputOrSuspend opStore 1 (take 1 input) 1
              4 -> proceed opOutput 1
              5 -> proceed opJumpIfTrue 2 
              6 -> proceed opJumpIfFalse 2 
              7 -> proceed opLessThan 3
              8 -> proceed opEqual 3
              9 -> proceed opModifyBase 1
 
              99 -> halt tape output ip input
              -1 -> err "Out of CPU cycles"

              _ -> error ("Unknown opcode " ++ show op)


get :: Int -> Tape -> Int -> [Int]
get n tape ip = traceable ( map retrieve (map (+ ip) [0..(n-1)]) )
  where 
    retrieve
      | ip < capacity tape = Seq.index $ mem tape
      | otherwise          = \x -> Map.findWithDefault 0 x $ extMem tape

getOne :: Tape -> Int -> Int
getOne tape ip = head $ get 1 tape ip

set :: Tape -> Int -> Int -> Tape
set tape ix val 
  | ix < capacity tape  = newTapeFull (updateMem (mem tape) ix val) (extMem tape) (baseOffset tape)
  | otherwise           = newTapeFull (mem tape) (updateExtMem (extMem tape) ix val) (baseOffset tape)

updateMem :: Seq Int -> Int -> Int -> Seq Int
updateMem mem ix val = Seq.update ix val mem

updateExtMem :: Map Int Int -> Int -> Int -> Map Int Int
updateExtMem extMem ix val = Map.insert ix val extMem

advance :: Int -> Int -> Maybe Int -> Int
advance ip n override = case override of 
  Just x  -> x
  Nothing -> ip + n

getParams :: Int -> Tape -> Int -> [Int] -> [Param]
getParams n tape ip aux = zipWith newParam modes vals
  where
    vals = (get n tape (ip+1)) ++ aux
    modes = getModes (n + (length aux)) $ getOne tape ip

getOp :: Int -> Int
getOp instr
  | instr < 100 = instr
  | otherwise  = read $ reverse $ take 2 (reverse $ show instr)
 
getModes :: Int -> Int -> [Int]
getModes n instr = map (read . (:[])) $ take n modeString
  where
    modeString = drop 2 $ ((reverse $ show instr) ++ (repeat '0'))

newParam :: Int -> Int -> Param
newParam mode val = case mode of 
  0 -> Positional val
  1 -> Immediate val
  2 -> Relative val
  _   -> error ("Unknown parameter mode: " ++ (show mode))

resolveParamAddress :: Tape -> Param -> Int
resolveParamAddress tape param = case param of
  Positional x -> x
  Immediate  x -> error ("Cannot resolve address of immediate parameter (" ++ show x ++ ")")
  Relative   x -> x + baseOffset tape

resolveParam :: Tape -> Param -> Int
resolveParam tape param = case param of 
  Positional _ -> getOne tape $ resolveParamAddress tape param
  Relative   _ -> getOne tape $ resolveParamAddress tape param
  Immediate  x -> x

paramValue :: Param -> Int
paramValue param = case param of 
  Positional x -> x
  Immediate  x -> x
  Relative   x -> x

opAdd :: Tape -> [Param] -> OpResult
opAdd tape arg = res (naryIndexedOp 2 tape arg sum) [] Nothing

opMult :: Tape -> [Param] -> OpResult
opMult tape arg = res (naryIndexedOp 2 tape arg product) [] Nothing
                  
opStore :: Tape -> [Param] -> OpResult
opStore tape arg = res (set tape (resolveParamAddress tape $ head arg) 
                                 (paramValue $ head $ drop 1 arg)) [] Nothing
 
opOutput :: Tape -> [Param] -> OpResult
opOutput tape arg = res tape [resolveParam tape $ head arg] Nothing

opJumpIf :: (Int -> Bool) -> Tape -> [Param] -> OpResult
opJumpIf test tape arg
  | testResult = res tape [] $ Just (resolveParam tape $ head $ drop 1 arg)
  | otherwise  = res tape [] Nothing
  where
    testResult = test (resolveParam tape $ head arg)

opJumpIfTrue :: Tape -> [Param] -> OpResult
opJumpIfTrue = opJumpIf (/= 0)

opJumpIfFalse :: Tape -> [Param] -> OpResult
opJumpIfFalse = opJumpIf (== 0)

opTest :: ([Int] -> Int) -> Tape -> [Param] -> OpResult
opTest test tape arg = res (naryIndexedOp 2 tape arg test) [] Nothing

opLessThan :: Tape -> [Param] -> OpResult
opLessThan = opTest (\(x:y:xs) -> if x < y then 1 else 0)

opEqual :: Tape -> [Param] -> OpResult
opEqual = opTest (\(x:y:xs) -> if x == y then 1 else 0)

opModifyBase :: Tape -> [Param] -> OpResult
opModifyBase tape arg = res (newTapeFull (mem tape) (extMem tape) (baseOffset tape + offset)) [] Nothing
  where
    offset = resolveParam tape $ head arg

res :: Tape -> [Int] -> Maybe Int -> OpResult
res tape outputs newIp = (tape, outputs, newIp)

-- Accepts (n+1) args [0..n] for an n-ary function, storing result in the nth arg
naryIndexedOp :: Int -> Tape -> [Param] -> ([Int] -> Int) -> Tape
naryIndexedOp n tape arg f = set 
                  tape
                  (resolveParamAddress tape $ arg !! n)
                  (f (map (resolveParam tape) (take n arg)))
                  
newState :: ExecState -> Tape -> [Int] -> Int -> [Int] -> State
newState exec tape output ip input = State { execState=exec, tapeState=tape, outputState=output, ipState=ip, inputState=input }

ok :: Tape -> [Int] -> Int -> [Int] -> Result
ok = validState Running 

halt :: Tape -> [Int] -> Int -> [Int] -> Result
halt = validState Halt 

suspend :: Tape -> [Int] -> Int -> [Int] -> Result
suspend = validState Suspended

validState :: ExecState -> Tape -> [Int] -> Int -> [Int] -> Result
validState exec tape output ip input = Right $ newState exec tape output ip input

err :: String -> Result
err e = Left e

primeTape :: [(Int, Int)] -> Tape -> Tape
primeTape vals tape = foldl (\t x -> (set t (fst x) (snd x))) tape vals

newTape :: [Int] -> Tape
newTape x = newTapeFull (Seq.fromList x) Map.empty 0

newTapeFull :: Seq Int -> Map Int Int -> Int -> Tape
newTapeFull mem extMem baseOffset = Tape { mem=mem, capacity=length mem, extMem=extMem, baseOffset=baseOffset }


traceable :: Show a => a -> a
traceable x
  | enableTrace = ioTraceDirect x
  | otherwise   = x

parseInput :: String -> Tape
parseInput input = newTape $ map read (wordsWhen (== ',') input)

                   
testProgram :: [Int] -> [Int] -> [Int] -> [Int] -> ()
testProgram prog input expTape expOutput = testProgramExt (newTape prog) input (newTape expTape) expOutput

testProgramExt :: Tape -> [Int] -> Tape -> [Int] -> ()
testProgramExt tape input expTape expOutput = case result of 
  Left e -> error ("Test failed: " ++ e)
  Right state -> head [
    assertEqual (tapeState state) expTape,
    assertEqual (outputState state) expOutput]
  
  where result = execute tape input

testProgramOutput :: [Int] -> [Int] -> [Int] -> ()
testProgramOutput prog input expOutput = case result of 
  Left e -> error ("Test failed: " ++ e)
  Right state -> assertEqual (outputState state) expOutput
  
  where result = execute (newTape prog) input

-- == Tests == --

cpuTests = [ basicTapeTest1, basicTapeTest2, basicTapeTest3, basicTapeTest4, primeTest
           , testAdd, testMult, testStore, testOutput1, testOutput2, testInputOutput
           , testJumpIfTrue1, testJumpIfTrue2, testJumpIfFalse1, testJumpIfFalse2
           , testModeDerivation, testOpcodeDeriv1, testOpcodeDeriv2, testOpcodeDeriv3, testOpcodeDeriv4
           , testPositional, testImmediate, testNegativeValues
           , testPositionalJumps1, testPositionalJumps2, testImmediateJumps1, testImmediateJumps2
           , testLessThanPositional1, testLessThanPositional2, testLessThanImmediate1, testLessThanImmediate2
           , testEqualityPositional1, testEqualityPositional2, testEqualityImmediate1, testEqualityImmediate2
           , testBranchJumps1, testBranchJumps2, testBranchJumps3, testModifyBase1, testModifyBase2 
           , testExtendedMemory1, testExtendedMemory2, testExtendedMemory3, testExtendedMemory4, testExtendedMemory5
           , testMemoryExtensions1, testMemoryExtensions2, testMemoryExtensions3 ]

-- Basic

basicTapeTest1 _ = tapeOnlyTest [1,0,0,0,99] [2,0,0,0,99]
basicTapeTest2 _ = tapeOnlyTest [2,3,0,3,99] [2,3,0,6,99]
basicTapeTest3 _ = tapeOnlyTest [2,4,4,5,99,0] [2,4,4,5,99,9801]
basicTapeTest4 _ = tapeOnlyTest [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

primeTest _ = assertEqual (primeTape [(1,2),(3,4),(5,6)] (newTape [0,0,0,0,0,0,0,0])) (newTape [0,2,0,4,0,6,0,0])

-- Opcodes

testAdd _ = assertEqual (opAdd (newTape [1,0,0,0]) [Positional 0, Positional 0, Positional 0]) $ res (newTape [2,0,0,0]) [] Nothing

testMult _ = assertEqual (opMult (newTape [2,0,0,0]) [Positional 0, Positional 0, Positional 2]) $ res (newTape [2,0,4,0]) [] Nothing

testStore _ = assertEqual (opStore (newTape [0,0,0,0]) [Positional 3, Positional 12]) $ res (newTape [0,0,0,12]) [] Nothing

testOutput1 _ = assertEqual (opOutput (newTape [1,2,3,4]) [Positional 1]) $ res (newTape [1,2,3,4]) [2] Nothing

testOutput2 _ = assertEqual (opOutput (newTape [1,2,3,4]) [Immediate 1]) $ res (newTape [1,2,3,4]) [1] Nothing

testInputOutput _ = testProgram [3,0,4,0,99] [12] [12,0,4,0,99] [12]

testJumpIfTrue1 _ = assertEqual (opJumpIfTrue (newTape [5,2,1,99]) [Positional 2, Positional 1]) $ res (newTape [5,2,1,99]) [] (Just 2)
testJumpIfTrue2 _ = assertEqual (opJumpIfTrue (newTape [5,2,1,99]) [Immediate 0, Positional 1]) $ res (newTape [5,2,1,99]) [] Nothing

testJumpIfFalse1 _ = assertEqual (opJumpIfFalse (newTape [5,2,0,99]) [Immediate 2, Positional 1]) $ res (newTape [5,2,0,99]) [] Nothing
testJumpIfFalse2 _ = assertEqual (opJumpIfFalse (newTape [5,2,0,99]) [Positional 2, Positional 3]) $ res (newTape [5,2,0,99]) [] (Just 99)

testModifyBase1 _ = assertEqual (opModifyBase (newTape [109,24,99]) [Immediate 24]) $ res 
                                              (newTapeFull (Seq.fromList [109,24,99]) Map.empty 24) [] Nothing
testModifyBase2 _ = assertEqual (opModifyBase (newTapeFull (Seq.fromList [9,4,99,4,8,12]) Map.empty 16) [Positional 4]) $ res
                                              (newTapeFull (Seq.fromList [9,4,99,4,8,12]) Map.empty 24) [] Nothing 
testModifyBase3 _ = assertEqual (opModifyBase (newTapeFull (Seq.fromList [9,4,99,4,8,12]) Map.empty 16) [Positional 4]) $ res
                                              (newTapeFull (Seq.fromList [9,4,99,4,8,12]) Map.empty 24) [] Nothing 

-- Instruction parsing

testModeDerivation _ = assertEqual (getModes 4 101002) [0,1,0,1]

testOpcodeDeriv1 _ = assertEqual (getOp 1) 1
testOpcodeDeriv2 _ = assertEqual (getOp 12) 12
testOpcodeDeriv3 _ = assertEqual (getOp 1003) 3
testOpcodeDeriv4 _ = assertEqual (getOp 101010021) 21

testPositional _ = testProgram [2,0,2,5,99,0] [] [2,0,2,5,99,4] []
testImmediate _ = testProgram [1002,4,3,4,33] [] [1002,4,3,4,99] []

testNegativeValues _ = testProgram [1101,100,-1,4,0] [] [1101,100,-1,4,99] []


tapeOnlyTest :: [Int] -> [Int] -> ()
tapeOnlyTest input exp = testProgram input [] exp []

-- Extended memory space
testExtendedMemory1 _ = testProgramExt (newTapeFull (Seq.fromList [1101,2,4,1,1102,2,4,1024,99]) Map.empty 0) []
                                       (newTapeFull (Seq.fromList [1101,6,4,1,1102,2,4,1024,99]) (Map.fromList [(1024,8)]) 0) []

testExtendedMemory2 _ = testProgramExt (newTapeFull (Seq.fromList [4,36,99,2,3]) (Map.fromList [(36,16)]) 0) []
                                       (newTapeFull (Seq.fromList [4,36,99,2,3]) (Map.fromList [(36,16)]) 0) [16]

testExtendedMemory3 _ = testProgramExt (newTapeFull (Seq.fromList [1102]) (Map.fromList [(1,4),(2,2),(3,0),(4,99)]) 0) []
                                       (newTapeFull (Seq.fromList [8]) (Map.fromList [(1,4),(2,2),(3,0),(4,99)]) 0) []

testExtendedMemory4 _ = testProgramExt (newTapeFull (Seq.fromList [109,1000,203,0,99]) Map.empty 0) [1]
                                       (newTapeFull (Seq.fromList [109,1000,203,0,99]) (Map.fromList [(1000,1)]) 1000) []

testExtendedMemory5 _ = testProgramExt (newTapeFull (Seq.fromList [1102]) (Map.fromList [(1,4),(2,2),(3,0),(4,99)]) 0) []
                                       (newTapeFull (Seq.fromList [8]) (Map.fromList [(1,4),(2,2),(3,0),(4,99)]) 0) []

-- Conditional jumps

testPositionalJumps1 _ = testProgram [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0] [3,12,6,12,15,1,13,14,13,4,13,99,0,0,1,9] [0]
testPositionalJumps2 _ = testProgram [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [12] [3,12,6,12,15,1,13,14,13,4,13,99,12,1,1,9] [1]
testImmediateJumps1 _ = testProgram [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0] [3,3,1105,0,9,1101,0,0,12,4,12,99,0] [0]
testImmediateJumps2 _ = testProgram [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [(-12)] [3,3,1105,-12,9,1101,0,0,12,4,12,99,1] [1]

-- Branch conditions

testLessThanPositional1 _ = testProgram [3,9,7,9,10,9,4,9,99,-1,8] [4] [3,9,7,9,10,9,4,9,99,1,8] [1]
testLessThanPositional2 _ = testProgram [3,9,7,9,10,9,4,9,99,-1,8] [9] [3,9,7,9,10,9,4,9,99,0,8] [0]

testLessThanImmediate1 _ = testProgram [3,3,1107,-1,8,3,4,3,99] [4] [3,3,1107,1,8,3,4,3,99] [1]
testLessThanImmediate2 _ = testProgram [3,3,1107,-1,8,3,4,3,99] [9] [3,3,1107,0,8,3,4,3,99] [0]

testEqualityPositional1 _ = testProgram [3,9,8,9,10,9,4,9,99,-1,8] [8] [3,9,8,9,10,9,4,9,99,1,8] [1]
testEqualityPositional2 _ = testProgram [3,9,8,9,10,9,4,9,99,-1,8] [6] [3,9,8,9,10,9,4,9,99,0,8] [0]

testEqualityImmediate1 _ = testProgram [3,3,1108,-1,8,3,4,3,99] [8] [3,3,1108,1,8,3,4,3,99] [1]
testEqualityImmediate2 _ = testProgram [3,3,1108,-1,8,3,4,3,99] [2] [3,3,1108,0,8,3,4,3,99] [1]

-- Multi-operation tests

testBranchJumps1 _ = testProgramOutput [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [7] [999]
testBranchJumps2 _ = testProgramOutput [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [8] [1000]
testBranchJumps3 _ = testProgramOutput [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [9] [1001]

-- Multi-operation tests of memory & offset extension
testMemoryExtensions1 _ = testProgramOutput [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] [] [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
testMemoryExtensions2 _ = testProgramOutput [1102,34915192,34915192,7,4,7,99,0] [] [1219070632396864]
testMemoryExtensions3 _ = testProgramOutput [104,1125899906842624,99] [] [1125899906842624]

