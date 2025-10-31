module Main (main) where
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Exception (evaluate)
import Integration.Sequential (integrateEpsilon)
import Integration.Parallel (parIntegrateEpsilon)

f :: Double -> Double
f x = sin x * cos x * exp (x / 1000) / (1 + x * x)  -- Складніша функція

a_const, b_const, epsilon :: Double
a_const = 0.0
b_const = 6000.0
epsilon = 1e-8  -- Менша epsilon = більше кроків

numCores :: Int
numCores = 8

main :: IO ()
main = do
    putStrLn "--- Haskell Parallel Integral (Epsilon-based) ---"
    printf "Function: sin(x)\nInterval: [%.1f, %.1f]\nEpsilon: %e\n" a_const b_const epsilon
    putStrLn "------------------------------------------------"
    
    -- Послідовний
    putStrLn "Running sequential version..."
    startSeq <- getCurrentTime
    let (resultSeq, stepsSeq) = integrateEpsilon f a_const b_const epsilon
    _ <- evaluate resultSeq
    endSeq <- getCurrentTime
    printf "Sequential: %f (steps: %d)\n" resultSeq stepsSeq
    printf "Time: %s\n\n" (show (diffUTCTime endSeq startSeq))
    
    -- Паралельний
    putStrLn $ "Running parallel version (" ++ show numCores ++ " cores)..."
    startPar <- getCurrentTime
    let (resultPar, stepsPar) = parIntegrateEpsilon numCores f a_const b_const epsilon
    _ <- evaluate resultPar
    endPar <- getCurrentTime
    printf "Parallel: %f (steps: %d)\n" resultPar stepsPar
    printf "Time: %s\n" (show (diffUTCTime endPar startPar))
    
    let timeSeq = realToFrac (diffUTCTime endSeq startSeq) :: Double
    let timePar = realToFrac (diffUTCTime endPar startPar) :: Double
    printf "\nSpeedup: %.2fx\n" (timeSeq / timePar)