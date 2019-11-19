module Lib
( Ising
, model
, randModel
, runMC
) where

import Control.Monad.State
import System.Random (StdGen, Random, mkStdGen, random, randomR, randomRs)
import Numeric.LinearAlgebra (Matrix(..), (><), accum, atIndex)
import Debug.Trace

type Ising = State IsingState

data IsingState = IsingState
     { dim :: Int
     , model :: Matrix Float
     , rng :: StdGen
     } deriving (Show)

runMC :: Ising ()
runMC = do
    e1 <- getEnergy
    flipRandSpin
    e2 <- getEnergy

    return ()

getEnergy :: Ising Float
getEnergy = undefined

flipRandSpin :: Ising ()
flipRandSpin = do
    state <- get
    let n = dim state
    row <- randR (0,n-1)
    col <- randR (0,n-1)
    flipSpin (row, col)

flipSpin :: (Int, Int) -> Ising ()
flipSpin (row, col) = do
    state <- get
    let spin = (model state) `atIndex` (row, col)
    let model' = accum (model state) (\a b -> b) [((row,col), if spin == 1 then 0 else 1)]
    put $ state { model = model' }
    return ()

newModel :: Int -> Int -> [Int] -> IsingState
newModel n seed spins =
    let fspins = map fromIntegral spins
        r = mkStdGen seed
    in IsingState n ((n><n) fspins) r

randModel n seed =
    let r = mkStdGen seed
    in newModel n seed $ take (n*n) $ randomRs (0,1) r

randR :: (Random a) => (a,a) -> Ising a
randR (a,b) = do 
    state <- get
    let (r, rng') = randomR (a,b) $ rng state
    put $ state { rng = rng' }
    return r

rand :: (Random a) => Ising a
rand = do 
    state <- get
    let (r, rng') = random $ rng state
    put $ state { rng = rng' }
    return r
