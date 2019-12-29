module Common.Cpu
( execute
, executeFor
, executeNoInput
, primeTape
, parseInput
, tapeState
, outputState

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

type Tape = Seq Int
data State = State { tapeState    :: Tape
                   , outputState  :: [Int] 
                   }
type Result = Either String State

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
            | cpuTime > 0 = Seq.index tape ip
            | otherwise   = -1
          
          proceedWithAuxInput = \f narg aux -> 
            let (opTape, opOutput) = (f tape ((get narg tape (ip+1)) ++ aux))
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

opAdd :: Tape -> [Int] -> (Tape, [Int])
opAdd tape arg = (naryIndexedOp 2 tape arg sum, [])

opMult :: Tape -> [Int] -> (Tape, [Int])
opMult tape arg = (naryIndexedOp 2 tape arg product, [])
                  
opStore :: Tape -> [Int] -> (Tape, [Int])
opStore tape arg = (set tape (head arg) (head $ tail arg), [])
 
opOutput :: Tape -> [Int] -> (Tape, [Int])
opOutput tape arg = (tape, [getOne tape (arg !! 0)])

-- Accepts (n+1) args [0..n] for an n-ary function, storing result in the nth arg
naryIndexedOp :: Int -> Tape -> [Int] -> ([Int] -> Int) -> Tape
naryIndexedOp n tape arg f = set 
                  tape
                  (arg !! n)
                  (f (map (Seq.index tape) (take n arg)))
                  
newState :: Tape -> [Int] -> State
newState tape output = State { tapeState=tape, outputState=output }

ok :: Tape -> [Int] -> Result
ok tape output = Right $ newState tape output

err :: String -> Result
err e = Left e

primeTape :: [(Int, Int)] -> Tape -> Tape
primeTape vals tape = foldl (\t x -> (set t (fst x) (snd x))) tape vals
  
parseInput :: String -> Tape
parseInput input = Seq.fromList $ map read (wordsWhen (== ',') input)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'
                    
nounVerbPairs :: Int -> Int -> [(Int, Int)]
nounVerbPairs l h = [ (x,y) | x <- [l..h], y <- [l..h] ]


