module Main where

import Lib

import Control.Monad.State

main :: IO ()
main = do
    let n = 5000
    let seed = 1000
    let model = randModel 20 seed
    print model
    let (x, model1) = runState (replicateM n runMC) model
    print $ model1
    let (x, model2) = runState (replicateM n runMC) model1
    print $ model2
    let (x, model3) = runState (replicateM n runMC) model2
    print $ model3
    let (x, model4) = runState (replicateM n runMC) model3
    print $ model4

    return ()
