module Lib
( Ising
, model
, randModel
, runMC
) where

import Control.Monad.State
import System.Random (StdGen, Random, mkStdGen, random, randomR, randomRs)
import Numeric.LinearAlgebra (Matrix(..), (><), accum, atIndex, toLists)
import Debug.Trace

type Ising = State IsingState

type Spin = Float

data IsingState = IsingState
     { dim :: Int
     , j :: Float
     , step :: Int
     , e :: Float
     , model :: Matrix Spin
     , rng :: StdGen
     } deriving (Show)

-- instance Show IsingState where
--     show (IsingState {d, j, s, m, r}) =
--         "IsingState { d = " ++ show d
--             ++ ", j = " ++ show j
--             ++ ", step = " ++ show step
--             ++ ", model = " ++ disp 0 m
--             ++ ", rng = " ++ show r
--             ++ " }"

runMC :: Ising Float
runMC = do
    state <- get
    put $ state { step = step state + 1}
    state <- get
    (row, col) <- flipRandSpin
    state <- get
    dE <- getFlipEnergy (row, col)
    state <- get

    if traceShowId dE <= 0 then return () -- accept move <=> do nothing
    else do
        r <- randR (0,1)

        if r <= exp (-beta * dE) then return () -- accept move <=> do nothing
        else do
            put state              -- reset the state to before the spin flip
            r <- rand :: Ising Int -- increment RNG
            return ()

    e' <- getTotalEnergy
    put $ state { e = traceShowId e' }
    return e'

getTotalEnergy :: Ising Float
getTotalEnergy = do
    state <- get
    let n = dim state
    neighbors <- mapM getNeighbors [(row, col) | row <- [0..n-1], col <- [0..n-1]]
    let spins = concat . toLists $ model state
    return $ sum [sum [si * sj | sj <- si_neighbors] | si <-spins, si_neighbors <- neighbors]

getFlipEnergy :: (Int, Int) -> Ising Float
getFlipEnergy (row, col) = do
    state <- get
    neighbors <- getNeighbors (row, col)
    return $ 2 * j state * (sum neighbors)

getNeighbors :: (Int, Int) -> Ising [Spin]
getNeighbors (row, col) = do
    state <- get
    let n = dim state
    let m = model state
    let up    = if row <= 0     then [] else [m `atIndex` (row - 1, col)]
    let down  = if row >= n - 1 then [] else [m `atIndex` (row + 1, col)]
    let left  = if col <= 0     then [] else [m `atIndex` (row, col - 1)]
    let right = if col >= n - 1 then [] else [m `atIndex` (row, col + 1)]
    return $ up ++ down ++ left ++ right

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
    let model' = accum (model state) (\a b -> b) [((row,col), if spin == 1 then 0 else 1)]
    put $ state { model = model' }
    return ()

newModel :: Int -> Int -> [Int] -> IsingState
newModel n seed spins =
    let fspins = map fromIntegral spins
        r = mkStdGen seed
    in IsingState n 2.0 0 0.0 ((n><n) fspins) r

randModel n seed =
    let r = mkStdGen seed
        spins = take (n*n) $ randomRs (0,1) r :: [Int]
    in newModel n seed $ [if s == 0 then -1 else s | s <- spins]

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
