module Integration.Parallel
    ( parIntegrate
    ) where

import Control.Parallel.Strategies (rdeepseq, using, parList)
import Integration.Sequential (integrate)

-- "Менеджер" для паралельного розрахунку
-- k - кількість потоків (шматків)
parIntegrate :: Int -> (Double -> Double) -> Double -> Double -> Int -> Double
parIntegrate k func a b n =
    let
        totalRange = b - a
        chunkSize  = totalRange / fromIntegral k -- Розмір інтервалу на один потік
        nPerChunk  = n `div` k                   -- Кількість кроків на один потік

        -- Створюємо список "завдань"
        -- Кожен елемент - це пара (a_i, b_i) для одного "шматка"
        intervals = [ (a + fromIntegral i * chunkSize, a + fromIntegral (i+1) * chunkSize)
                    | i <- [0..k-1] ]

        -- Створюємо список "лінивих" завдань.
        -- Використовуємо `integrate` з нашого іншого модуля
        tasks = [ integrate func ai bi nPerChunk | (ai, bi) <- intervals ]

        -- Магія паралелізму
        parallelResults = tasks `using` parList rdeepseq

    in
        -- Просто сумуємо результати з усіх потоків
        sum parallelResults
