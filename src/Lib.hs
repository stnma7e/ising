module Lib
( Ising
, randModel
, runBatch
, runMC
, totalEnergy
, totalMagnetization
, flipRandSpin
, flipSpin
) where

import Model

import Data.Maybe
import Control.Monad.State
import System.Random (Random, random, randomR)
import Numeric.LinearAlgebra (accum, atIndex, toLists)
import Debug.Trace

runBatch :: Show a => Int -> [Ising a] -> Ising ([Maybe [a]], IsingState)
runBatch steps prop = do
    props <- replicateM steps $ runMC prop
    state <- get
    return (props, state)

runMC :: [Ising a] -> Ising (Maybe [a])
runMC properties = do
    incrementStep
    state_i <- get
    dE <- flipRandSpin >>= getSpinEnergy >>= (\x -> return $ -x)
    state_f <- get

    if dE <= 0
        then put $ state_f { nAccept = nAccept state_f + 1 }
    else do
        r <- randR (0,1)

        if r <= exp (-beta * dE)
            then put $ state_f { nAccept = nAccept state_f + 1 }
        else do
            put state_i            -- reset the state to before the spin flip
            r <- rand :: Ising Int -- increment RNG
            return ()

    state <- get
    if step state `mod` propFreq state == 0
        then liftM Just $ sequence properties
        else return Nothing

totalEnergy :: Ising Float
totalEnergy = do
    state <- get
    let n = dim state
    let indices = [(row, col) | row <- [0..n-1], col <- [0..n-1]]
    liftM sum $ mapM getSpinEnergy indices

totalMagnetization :: Ising Float
totalMagnetization = do
        state <- get
        return $ sum . concat . toLists $ model state

getSpinEnergy :: (Int, Int) -> Ising Float
getSpinEnergy (row, col) = do
    state <- get
    neighbors <- getNeighbors (row, col)
    let spin = model state `atIndex` (row,col)
    return $ 2 * j state * spin * (sum neighbors)

getNeighbors :: (Int, Int) -> Ising [Spin]
getNeighbors (row, col) = do
    state <- get
    let n = dim state
    let m = model state
    let ns = map (atIndex m) [ ((row - 1) `mod` n, col)
                             , ((row + 1) `mod` n, col)
                             , (row, (col - 1) `mod` n)
                             , (row, (col + 1) `mod` n)
                             ]
    return ns

flipRandSpin :: Ising (Int, Int)
flipRandSpin = do
    state <- get
    let n = dim state
    row <- randR (0,n-1)
    col <- randR (0,n-1)
    flipSpin (row, col)
    return (row, col)

flipSpin :: (Int, Int) -> Ising ()
flipSpin (row, col) = do
    state <- get
    let spin = (model state) `atIndex` (row, col)
    let model' = accum (model state) (\a b -> a) [((row,col), -spin)]
    put $ state { model = model' }

-----
--
-- Helper functions
--
-----

beta = 1.0

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

incrementStep :: Ising Int
incrementStep = do
    state <- get
    let step' = step state + 1
    put $ state { step = step' }
    return step'
