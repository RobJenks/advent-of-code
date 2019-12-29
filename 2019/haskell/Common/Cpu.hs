module Common.Cpu
( execute
, executeFor
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


execute :: Tape -> Result
execute tape = executeFor tape maxCycles

executeFor :: Tape -> Int -> Result
executeFor tape cpuCycles = step (ok tape []) 0 cpuCycles

step :: Result -> Int -> Int -> Result
step input ip cpuTime = result
  where 
    result = case input of 
      Left e -> err e
      Right state -> cycleResult
        where 
          tape = tapeState state
          output = outputState state
          op 
            | cpuTime > 0 = Seq.index tape ip
            | otherwise   = -1
          proceed = (\f narg -> step (ok (f tape (get narg tape (ip+1))) output) (ip+narg+1) (cpuTime-1))
          cycleResult =
            case op of
              1 -> proceed opAdd 3
              2 -> proceed opMult 3 
           
              99 -> ok tape output
              -1 -> err "Out of CPU cycles"

              _ -> error ("Unknown opcode " ++ show op)


get :: Int -> Tape -> Int -> [Int]
get n tape ip = map (Seq.index tape) (map (+ ip) [0..(n-1)])

set :: Tape -> Int -> Int -> Tape
set tape ix val = Seq.update ix val tape

opAdd :: Tape -> [Int] -> Tape
opAdd tape arg = binaryIndexedOp tape arg sum

opMult :: Tape -> [Int] -> Tape
opMult tape arg = binaryIndexedOp tape arg product
                   
binaryIndexedOp :: Tape -> [Int] -> ([Int] -> Int) -> Tape
binaryIndexedOp tape arg f = set 
                  tape
                  (arg !! 2)
                  (f (map (Seq.index tape) (take 2 arg)))
                  
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


