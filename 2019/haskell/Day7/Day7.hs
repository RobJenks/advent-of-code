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
  res <- runPipelineThreads runPipeline (Cpu.parseInput x) (enumeratePhases [0..4])
  return $ show $ foldl max 0 res


part2 :: String -> IO String
part2 x = do
  res <- runPipelineThreads runPipelineCyclic (Cpu.parseInput x) (enumeratePhases [5..9])
  return $ show $ foldl max 0 res



runPipelineThreads :: (Tape -> [Int] -> Int) -> Tape -> [[Int]] -> IO [Int]
runPipelineThreads fn tape phases
  | (null phases) = do return []
  | otherwise     = do
      res <- mapConcurrently (io.fn tape) (take maxWorkers phases)
      next <- runPipelineThreads fn tape $ drop maxWorkers phases
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
      otherwise -> runPipelineCyclicFromState $ map (\x -> either error id (Cpu.executeFromState x)) cycledStates
      where
        cycledStates = cycleStates states


prepareStates :: Tape -> [Int] -> [Cpu.State]
prepareStates tape phases = map (\x -> Cpu.newState Cpu.Running tape (snd x) 0 [fst x]) comps
  where
    comps = reverse (zipPadded 999 [] (reverse phases) [[0]])

cycleStates :: [Cpu.State] -> [Cpu.State]
cycleStates states = zipWith (\s input -> Cpu.newState (Cpu.execState s) (Cpu.tapeState s) [] (Cpu.ipState s) ((Cpu.inputState s) ++ input)) states inputs
  where
    inputs = map Cpu.outputState $ rotate 1 states
    


enumeratePhases :: [Int] -> [[Int]]
enumeratePhases = permutations


-- Tests 
tests = [ testPhaseEnumeration, pipelineTest1, pipelineTest2, pipelineTest3,
          testPrepareStates, testCycleStates, testCyclicExecution1, testCyclicExecution2 ]

testPhaseEnumeration _ = assertEqual (length $ enumeratePhases [0,1,2]) 6

pipelineTest1 _ = assertEqual (testPipeline [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] [4,3,2,1,0]) 43210
pipelineTest2 _ = assertEqual (testPipeline [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] [0,1,2,3,4]) 54321
pipelineTest3 _ = assertEqual (testPipeline [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] [1,0,4,3,2]) 65210

testPrepareStates _ = assertEqual (prepareStates tape [1,2,4]) expected
  where
    tape = Cpu.newTape [10,20,30,40]
    expected = [testState tape [] [1], testState tape [] [2], testState tape [0] [4]]
 
testCycleStates _ = assertEqual (cycleStates states) expected
  where
    tape = Cpu.newTape [10,20,30,40]
    states = [testState tape [] [1], testState tape [5,6] [2,3], testState tape [7] [4]]
    expected = [testState tape [] [1,7], testState tape [] [2,3], testState tape [] [4,5,6]]

testCyclicExecution1 _ = assertEqual (testPipelineCyclic [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5] [9,8,7,6,5]) 139629729

testCyclicExecution2 _ = assertEqual (testPipelineCyclic [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10] [9,7,8,5,6]) 18216 



testPipeline :: [Int] -> [Int] -> Int
testPipeline prog phases = runPipeline (Cpu.newTape prog) phases

testPipelineCyclic :: [Int] -> [Int] -> Int
testPipelineCyclic prog phases = runPipelineCyclic (Cpu.newTape prog) phases

testState :: Tape -> [Int] -> [Int] -> Cpu.State
testState tape output input = Cpu.newState Cpu.Running tape output 0 input

