module Main where

import Lib
import Model

import Control.Monad.State
import System.Random

main :: IO ()
main = do
    let nStep = 9000000
    let snaps = 4
    let stepsPerFrame = nStep `div` snaps

    seed <- randomIO :: IO Int
    print seed

    let n = 60
    let j = 0.5

    let model = newModel seed n j $ downSpins n
    let model = randModel seed n j
    let model = newModel seed n j $ upSpins n
    print model

    let prop = getTotalEnergy
    let prop = return 0.0

    let (frames, model') = runState (replicateM snaps $ runBatch stepsPerFrame prop) model
    let energies = map fst frames
    let avgE = (sum energies) / fromIntegral (length energies)
    putStrLn $ showFrames frames
    putStrLn $ "Average E: " ++ show avgE

showFrames = foldl (\acc (prop, state) -> acc ++ show prop ++ "\n" ++ show state ++ "\n") ""
