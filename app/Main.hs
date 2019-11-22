module Main where

import Lib
import Model

import Control.Monad.State
import System.Random

main :: IO ()
main = do
    let total = 50000
    let snaps = 10
    let n = total `div` snaps
    seed <- randomIO :: IO Int
    print seed
    let model = randModel seed 20 1.0
    model' <- foldl (\acc x -> acc >>= runBatch n) (runBatch n model) [1..snaps]
    -- let (x, model3) = runState (replicateM n runMC) model2
    print model'
    return ()

runBatch :: Int -> IsingState -> IO IsingState
runBatch n model = do
    let model' = execState (replicateM n runMC) model
    print $ model'
    return model'
