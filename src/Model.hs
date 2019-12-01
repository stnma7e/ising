module Model where

import Control.Monad.State.Strict
--import Control.Monad.State
import System.Random (StdGen, mkStdGen, randomRs)
import Numeric.LinearAlgebra (Matrix(..), (><), toLists)

type Ising = State IsingState
type Spin = Double
type Model = Matrix Spin

data IsingState = IsingState
     { dim :: Int
     , j :: Double
     , h :: Double
     , step :: Int
     , nAccept :: Int
     , propFreq :: Int
     , model :: Model
     , rng :: StdGen
     }

instance Show IsingState where
    show state =
        "IsingState { d = " ++ show (dim state)
            ++ ", j = " ++ show (j state)
            ++ ", step = " ++ show (step state)
            ++ ", accept = " ++ show (nAccept state)
            ++ ", rng = " ++ show (rng state)
            ++ " }"
            ++ showModel (model state)

showModel :: Model -> String
showModel m =
    let rows = toLists m
        showRow r = "|" ++ foldl (\acc s -> acc ++ showSpin s) "" r ++ "|"
        showSpin s = if s == -1 then " " else "*"
        stringRows = map showRow rows
    in foldl (\acc r -> acc ++ "\n" ++ r) "" stringRows

newModel :: Int -> Int -> Double -> [Int] -> IsingState
newModel seed n j spins =
    let rng = mkStdGen seed
        model = (n><n) $ map fromIntegral spins
    in IsingState
        { dim = n
        , j = j
        , h = 0.0
        , step = 0
        , nAccept = 0
        , propFreq = 1000
        , model = model
        , rng = rng
        }

randModel seed n j =
    let r = mkStdGen seed
        spins = take (n*n) $ randomRs (0,1) r :: [Int]
    in newModel seed n j [if s == 0 then -1 else s | s <- spins]

upSpins   n = [ 1 | _ <- [1..n*n]]
downSpins n = [-1 | _ <- [1..n*n]]

setPropertyFreq :: Int -> IsingState -> IsingState
setPropertyFreq f model = model { propFreq = f }

setH :: Double -> IsingState -> IsingState
setH h model = model { h = h }
