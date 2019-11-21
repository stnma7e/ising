module Model where

import Control.Monad.State
import System.Random (StdGen, mkStdGen, randomRs)
import Numeric.LinearAlgebra (Matrix(..), (><), toLists)

type Ising = State IsingState
type Spin = Float
type Model = Matrix Spin

data IsingState = IsingState
     { dim :: Int
     , j :: Float
     , step :: Int
     , nAccept :: Int
     , e :: Float
     , model :: Model
     , rng :: StdGen
     }

instance Show IsingState where
    show state =
        "IsingState { d = " ++ show (dim state)
            ++ ", j = " ++ show (j state)
            ++ ", step = " ++ show (step state)
            ++ ", energy = " ++ show (e state)
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

newModel :: Int -> Int -> [Int] -> IsingState
newModel n seed spins =
    let rng = mkStdGen seed
        model = (n><n) $ map fromIntegral spins
        energy = 0.0
        j = 1.0
    in IsingState n j 0 0 energy model rng

randModel n seed =
    let r = mkStdGen seed
        spins = take (n*n) $ randomRs (0,1) r :: [Int]
    in newModel n seed $ [if s == 0 then -1 else s | s <- spins]

