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
)
where

import Data.Foldable
import Data.Either
import Data.List
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Control.Exception

import Common.Util


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
            in step (ok opTape (output ++ opOutput)) input (ip+narg+1) (cpuTime-1)
          
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

getParams :: Int -> Tape -> Int -> [Int] -> [Param]
getParams n tape ip aux = zipWith newParam modes vals
  where
    vals = (get n tape (ip+1)) ++ aux
    modes = getModes (n + (length aux)) $ getOne tape ip

getOp :: Int -> Int
getOp instr
  | instr < 10 = instr
  | otherwise  = read $ take 2 (show instr)
 
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
opOutput tape arg = (tape, [getOne tape (resolveParam tape $ head arg)])

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






