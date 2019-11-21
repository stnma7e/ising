module Lib
( Ising
, randModel
, runMC
, flipRandSpin
, flipSpin
) where

import Model

import Control.Monad.State
import System.Random (Random, random, randomR)
import Numeric.LinearAlgebra (accum, atIndex, toLists)
import Debug.Trace

runMC :: Ising ()
runMC = do
    incrementStep
    state_i <- get
    dE <- flipRandSpin >>= getFlipEnergy
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

    -- state <- get
    -- e <- getTotalEnergy
    -- put $ state { e = e }
    -- return e

getTotalEnergy :: Ising Float
getTotalEnergy = do
    state <- get
    let n = dim state
    neighbors <- mapM getNeighbors [(row, col) | row <- [0..n-1], col <- [0..n-1]]
    let spins = concat . toLists $ model state
    return $ sum [sum [si * sj | sj <- si_neighbors] | si <- spins, si_neighbors <- neighbors]

getFlipEnergy :: (Int, Int) -> Ising Float
getFlipEnergy (row, col) = do
    state <- get
    neighbors <- getNeighbors (row, col)
    let spin = model state `atIndex` (row,col)
    return $ 2 * j state * (-spin) * (sum neighbors)

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
