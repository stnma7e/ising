module Main where

import Lib
import Model

import Control.Monad.State.Strict
--import Control.Monad.State
import System.Random
import Debug.Trace

main :: IO ()
main = do
    let temps =  [0.5 | _ <- [1..10]] --[0.4017 + i * 0.0001 | i <- [1 .. 500]]
    results <- mapM run temps
    mapM print $ zip temps $ map snd results
    return ()

run :: Double -> IO ([IsingState], [Double])
run j = do
    let n = 40
    let nStep = 4000000
    let snaps = 4
    let nEq   = 1000000
    let stepsPerFrame = nStep `div` snaps

    seed <- randomIO :: IO Int
    --print seed

    let model = newModel seed n j $ downSpins n
    let model = newModel seed n j $ upSpins n
    let model = randModel seed n j
    --print model

    let prop = [ return 0.0 ] :: [Ising Double]
    let prop = [ totalEnergy
               , totalEnergy >>= square
               , totalMagnetization
               , totalMagnetization >>= square
               ]

    let (eqFrames, eqModel) = runState (runBatch nEq [return ()]) model

    let (frames, model') = runState (replicateM snaps $ runBatch stepsPerFrame prop) eqModel
    let props = map fst frames
    let states = map snd frames
    let avgs = computeAverage $ concat props
    --mapM print states
    --print avgs
    return (states, avgs)

computeAverage :: Fractional a => [Maybe [a]] -> [a]
computeAverage props =
    let startingProps = ([], replicate (length props) 0.0)
        properties    = fst $ foldr (\x (xs, acc) -> case x of
                                        Just x' -> (x':xs, x')
                                        Nothing -> (acc:xs, acc))
                                    startingProps
                                    props
        avg x = sum x / fromIntegral (length x)
    in map avg $ unzipN properties

square x = return $ x*x

unzipN :: [[a]] -> [[a]]
unzipN (x:xs) =
    let addToList (z, y) = y : z
        unzip' zs ys = foldr (\y z -> map addToList $ zip z y) zs ys
    in unzip' (map (\q -> [q]) x) xs
