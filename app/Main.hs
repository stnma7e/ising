module Main where

import Lib
import Model

import Data.Maybe
import Control.Monad.State
import System.Random

main :: IO ()
main = do
    let total = 1000000
    let snaps = 10
    let n = total `div` snaps
    seed <- randomIO :: IO Int
    print seed
    let model = setPropertyFreq 1000 $ randModel seed 20 2.5
    model' <- foldl (\acc _ -> acc >>= runBatch n) (runBatch n model) [1..snaps]
    return ()

runBatch :: Int -> IsingState -> IO IsingState
runBatch n model = do
    let (x, model') = runState (replicateM n $ runMC (liftM Just getTotalEnergy) Nothing) model
    let energies = map fromJust $ filter isJust x
    let avgE = (sum energies) / fromIntegral (length energies)
    print avgE
    print $ model'
    return model'
