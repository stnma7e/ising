module Main where

import Lib

import Control.Monad.State

main :: IO ()
main = do
    let seed = 1
    let model = randModel 10 seed
    print model
    let (x, model') = runState runMC model
    print $ model'
    print $ x

    return ()
