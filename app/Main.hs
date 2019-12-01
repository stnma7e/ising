module Main where

import Lib
import Model

import Control.Monad.State.Strict
--import Control.Monad.State
import System.Random
import Debug.Trace

data UpDown = Up | Down | Random
            deriving (Show)
newtype J = J Double
          deriving (Show)
newtype H = H Double 
          deriving (Show)
data System = System J UpDown H
            deriving (Show)

main :: IO ()
main = do
    let sys =  [System (J 0.5) upDown (H h) | upDown <- [Up, Down], h <- [0.25, 0.5, 1.0], _ <- [1..5]]
    results <- mapM run sys
    mapM print $ zip sys $ map snd results
    return ()

run :: System -> IO ([IsingState], [Double])
run (System (J j) upDown (H h)) = do
    let n = 40
    let nStep = 4000000
    let snaps = 4
    let nEq   = 1000000
    let stepsPerFrame = nStep `div` snaps

    seed <- randomIO :: IO Int
    --print seed

    let model = case upDown of
                    Up     -> newModel seed n j $ downSpins n
                    Down   -> newModel seed n j $ upSpins n
                    Random -> randModel seed n j
    --print model

    let prop = [ return 0.0 ] :: [Ising Double]
    let prop = [ totalEnergy
               , totalEnergy >>= square
               , totalMagnetization
               , totalMagnetization >>= square
               ]

    let (eqFrames, eqModel) = runState (runBatch nEq [return ()]) $ setH h model

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
