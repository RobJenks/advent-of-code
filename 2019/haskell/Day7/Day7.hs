module Day7.Day7
( part1
, part2
, tests
)
where

import Data.List
import Control.Concurrent.Async

import Common.Util
import Common.Cpu (Tape, Result)
import qualified Common.Cpu as Cpu


maxWorkers = 16 :: Int


part1 :: String -> IO String
part1 x = do
  res <- runPipelineThreads (Cpu.parseInput x) (enumeratePhases [0..4])
  return $ show $ foldl max 0 res


part2 :: String -> String
part2 x = "B"



runPipelineThreads :: Tape -> [[Int]] -> IO [Int]
runPipelineThreads tape phases
  | (null phases) = do return []
  | otherwise     = do
      res <- mapConcurrently (io.runPipeline tape) (take maxWorkers phases)
      next <- runPipelineThreads tape $ drop maxWorkers phases
      return (res ++ next)

      where work = take maxWorkers phases


runPipeline :: Tape -> [Int] -> Int
runPipeline tape phases = foldl (\res p -> either (error) (head . Cpu.outputState) (Cpu.execute tape [p,res])) 0 phases

runPipelineCyclic :: Tape -> [Int] -> Int
runPipelineCyclic tape phases = head $ Cpu.outputState $ last result
  where
    result = runPipelineCyclicFromState $ prepareStates tape phases


runPipelineCyclicFromState :: [Cpu.State] -> [Cpu.State]
runPipelineCyclicFromState states = result
  where
    finalState = Cpu.execState $ last states
    result = case finalState of  
      Cpu.Halt  -> states
      otherwise -> runPipelineCyclicFromState $ map (\x -> either error id (Cpu.executeFromState x)) states


prepareStates :: Tape -> [Int] -> [Cpu.State]
prepareStates tape phases = map (\x -> Cpu.newState Cpu.Running tape (snd x) 0 [fst x]) comps
  where
    prime = [[0]] ++ (repeat [])
    comps = reverse $ zip (reverse phases) prime

cycleStates :: [Cpu.State] -> [Cpu.State]
cycleStates states = zipWith (\s input -> Cpu.newState (Cpu.execState s) (Cpu.tapeState s) [] (Cpu.ipState s) ((Cpu.inputState s) ++ input)) states inputs
  where
    inputs = map Cpu.outputState $ rotate 1 states
    


enumeratePhases :: [Int] -> [[Int]]
enumeratePhases = permutations


-- Tests 
tests = [ testPhaseEnumeration, pipelineTest1, pipelineTest2, pipelineTest3 ]

testPhaseEnumeration _ = assertEqual (length $ enumeratePhases [0,1,2]) 6

pipelineTest1 _ = assertEqual (testPipeline [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0]) 43210
pipelineTest2 _ = assertEqual (testPipeline [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4]) 54321
pipelineTest3 _ = assertEqual (testPipeline [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2]) 65210



testPipeline :: [Int] -> [Int] -> Int
testPipeline prog phases = runPipeline (Cpu.newTape prog) phases


