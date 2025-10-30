module Main (main) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)
import Control.Exception (evaluate)

import Integration.Sequential (integrate)
import Integration.Parallel (parIntegrate)

-- Наша функція, яку ми будемо інтегрувати.
f :: Double -> Double
f x = sin x
-- f x = x * x -- інший варіант для тесту

a_const :: Double
a_const = 0.0 -- Початок інтервалу

b_const :: Double
b_const = 100.0 -- Кінець інтервалу 

n_const :: Int
n_const = 20000000 -- Кількість кроків (точність).

numCores :: Int
numCores = 8 -- Змініть на кількість ваших фізичних ядер


main :: IO ()
main = do
    putStrLn "--- Haskell Parallel Integral Calculation ---"
    printf "Function: sin(x)\nInterval: [%.1f, %.1f]\nSteps: %d\n" a_const b_const n_const
    putStrLn "---------------------------------------------"

    -- --- Послідовний запуск ---
    putStrLn $ "Running sequential version (1 core)..."
    startTimeSeq <- getCurrentTime

    let resultSeq = integrate f a_const b_const n_const
    _ <- evaluate resultSeq
    
    endTimeSeq <- getCurrentTime
    printf "Sequential result: %f\n" resultSeq
    printf "Sequential time:   %s\n\n" (show (diffUTCTime endTimeSeq startTimeSeq))


    -- --- Паралельний запуск ---
    putStrLn $ "Running parallel version (" ++ show numCores ++ " cores)..."
    startTimePar <- getCurrentTime

    let resultPar = parIntegrate numCores f a_const b_const n_const
    _ <- evaluate resultPar
    
    endTimePar <- getCurrentTime
    printf "Parallel result:   %f\n" resultPar
    printf "Parallel time:     %s\n" (show (diffUTCTime endTimePar startTimePar))
    putStrLn "---------------------------------------------"

    -- --- Демонстрація ефективності ---
    let timeSeq = realToFrac (diffUTCTime endTimeSeq startTimeSeq) :: Double
    let timePar = realToFrac (diffUTCTime endTimePar startTimePar) :: Double
    printf "Speedup: %.2fx\n" (timeSeq / timePar)
