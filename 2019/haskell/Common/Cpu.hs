module Common.Cpu
( execute
, executeFor
, executeNoInput
, primeTape
, newTape
, parseInput
, tapeState
, outputState
, testProgram

, Tape
, State

, cpuTests
)
where

import Data.Foldable
import Data.Either
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Exception

import Common.Util (assertEqual, wordsWhen)


type Tape = Seq Int
data State = State { tapeState    :: Tape
                   , outputState  :: [Int] 
                   }
type Result = Either String State
data Param = Positional Int | Immediate Int  deriving Show

maxCycles = 10000 :: Int


execute :: Tape -> Int -> Result
execute tape input = executeFor tape input maxCycles

executeFor :: Tape -> Int -> Int -> Result
executeFor tape input cpuCycles = step (ok tape []) input 0 cpuCycles

executeNoInput :: Tape -> Result
executeNoInput tape = execute tape 0

step :: Result -> Int -> Int -> Int -> Result
step inputState input ip cpuTime = result
  where 
    result = case inputState of 
      Left e -> err e
      Right state -> cycleResult
        where 
          tape = tapeState state
          output = outputState state
          op 
            | cpuTime > 0 = getOp $ getOne tape ip
            | otherwise   = -1
          
          proceedWithAuxInput = \f narg aux -> 
            let (opTape, opOutput) = f tape (getParams narg tape ip aux)
            in step (ok opTape (output ++ opOutput)) input (advance ip (narg+1)) (cpuTime-1)
          
          proceed f narg = proceedWithAuxInput f narg []

          cycleResult = 
            case op of
              1 -> proceed opAdd 3
              2 -> proceed opMult 3 
              3 -> proceedWithAuxInput opStore 1 [input]          
              4 -> proceed opOutput 1 
 
              99 -> ok tape output
              -1 -> err "Out of CPU cycles"

              _ -> error ("Unknown opcode " ++ show op)


get :: Int -> Tape -> Int -> [Int]
get n tape ip = map (Seq.index tape) (map (+ ip) [0..(n-1)])

getOne :: Tape -> Int -> Int
getOne tape ip = head $ get 1 tape ip

set :: Tape -> Int -> Int -> Tape
set tape ix val = Seq.update ix val tape

advance :: Int -> Int -> Int
advance ip n = ip + n

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
  _   -> error ("Unknown parameter mode: " ++ (show mode))

resolveParam :: Tape -> Param -> Int
resolveParam tape param = case param of 
  Positional x -> getOne tape x
  Immediate  x -> x

paramValue :: Param -> Int
paramValue p = case p of 
  Positional x -> x
  Immediate x -> x

opAdd :: Tape -> [Param] -> (Tape, [Int])
opAdd tape arg = (naryIndexedOp 2 tape arg sum, [])

opMult :: Tape -> [Param] -> (Tape, [Int])
opMult tape arg = (naryIndexedOp 2 tape arg product, [])
                  
opStore :: Tape -> [Param] -> (Tape, [Int])
opStore tape arg = (set tape (paramValue $ head arg) 
                             (paramValue $ head $ drop 1 arg), [])
 
opOutput :: Tape -> [Param] -> (Tape, [Int])
opOutput tape arg = (tape, [resolveParam tape $ head arg])

-- Accepts (n+1) args [0..n] for an n-ary function, storing result in the nth arg
naryIndexedOp :: Int -> Tape -> [Param] -> ([Int] -> Int) -> Tape
naryIndexedOp n tape arg f = set 
                  tape
                  (paramValue $ arg !! n)
                  (f (map (resolveParam tape) (take n arg)))
                  
newState :: Tape -> [Int] -> State
newState tape output = State { tapeState=tape, outputState=output }

ok :: Tape -> [Int] -> Result
ok tape output = Right $ newState tape output

err :: String -> Result
err e = Left e

primeTape :: [(Int, Int)] -> Tape -> Tape
primeTape vals tape = foldl (\t x -> (set t (fst x) (snd x))) tape vals

newTape :: [Int] -> Tape
newTape = Seq.fromList
 
parseInput :: String -> Tape
parseInput input = newTape $ map read (wordsWhen (== ',') input)

                   
testProgram :: [Int] -> Int -> [Int] -> [Int] -> ()
testProgram prog input expTape expOutput = case result of 
  Left e -> error ("Test failed: " ++ e)
  Right state -> head [
    assertEqual (tapeState state) (newTape expTape),
    assertEqual (outputState state) expOutput]
  
  where result = execute (newTape prog) input


-- == Tests == --

cpuTests = [ basicTapeTest1, basicTapeTest2, basicTapeTest3, basicTapeTest4, primeTest
           , testAdd, testMult, testStore, testOutput1, testOutput2, testInputOutput
           , testModeDerivation, testOpcodeDeriv1, testOpcodeDeriv2, testOpcodeDeriv3, testOpcodeDeriv4
           , testPositional, testImmediate, testNegativeValues ]


-- Basic

basicTapeTest1 _ = tapeOnlyTest [1,0,0,0,99] [2,0,0,0,99]
basicTapeTest2 _ = tapeOnlyTest [2,3,0,3,99] [2,3,0,6,99]
basicTapeTest3 _ = tapeOnlyTest [2,4,4,5,99,0] [2,4,4,5,99,9801]
basicTapeTest4 _ = tapeOnlyTest [1,1,1,4,99,5,6,0,99] [30,1,1,4,2,5,6,0,99]

primeTest _ = assertEqual (primeTape [(1,2),(3,4),(5,6)] (newTape [0,0,0,0,0,0,0,0])) (newTape [0,2,0,4,0,6,0,0])

-- Opcodes

testAdd _ = assertEqual (opAdd (newTape [1,0,0,0]) [Positional 0, Positional 0, Positional 0]) (newTape [2,0,0,0], [])

testMult _ = assertEqual (opMult (newTape [2,0,0,0]) [Positional 0, Positional 0, Positional 2]) (newTape [2,0,4,0], [])

testStore _ = assertEqual (opStore (newTape [0,0,0,0]) [Positional 3, Positional 12]) (newTape [0,0,0,12], [])

testOutput1 _ = assertEqual (opOutput (newTape [1,2,3,4]) [Positional 1]) (newTape [1,2,3,4], [2])

testOutput2 _ = assertEqual (opOutput (newTape [1,2,3,4]) [Immediate 1]) (newTape [1,2,3,4], [1])

testInputOutput _ = testProgram [3,0,4,0,99] 12 [12,0,4,0,99] [12]

-- Instruction parsing

testModeDerivation _ = assertEqual (getModes 4 101002) [0,1,0,1]

testOpcodeDeriv1 _ = assertEqual (getOp 1) 1
testOpcodeDeriv2 _ = assertEqual (getOp 12) 12
testOpcodeDeriv3 _ = assertEqual (getOp 1003) 3
testOpcodeDeriv4 _ = assertEqual (getOp 101010021) 21

testPositional _ = testProgram [2,0,2,5,99,0] 0 [2,0,2,5,99,4] []
testImmediate _ = testProgram [1002,4,3,4,33] 0 [1002,4,3,4,99] []

testNegativeValues _ = testProgram [1101,100,-1,4,0] 0 [1101,100,-1,4,99] []


tapeOnlyTest :: [Int] -> [Int] -> ()
tapeOnlyTest input exp = testProgram input 0 exp []

