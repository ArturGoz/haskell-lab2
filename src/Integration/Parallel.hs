module Integration.Parallel
    ( parIntegrateEpsilon
    , parIntegrate
    ) where
import Control.Parallel.Strategies (rdeepseq, using, parList)
import Integration.Sequential (integrate)

-- Кожне ядро самостійно уточнює свій інтервал
parIntegrateEpsilon :: Int -> (Double -> Double) -> Double -> Double -> Double -> (Double, Int)
parIntegrateEpsilon k func a b eps =
    let chunkSize = (b - a) / fromIntegral k
        intervals = [(a + fromIntegral i * chunkSize, a + fromIntegral (i+1) * chunkSize)
                    | i <- [0..k-1]]
        -- Кожне ядро окремо досягає epsilon на своєму інтервалі
        tasks = [refineChunk func ai bi (eps / fromIntegral k) | (ai, bi) <- intervals]
        results = tasks `using` parList rdeepseq
        totalResult = sum [r | (r, _) <- results]
        totalSteps = sum [n | (_, n) <- results]
    in (totalResult, totalSteps)
  where
    refineChunk f a' b' e = refine 100
      where
        refine n =
            let current = integrate f a' b' n
                next = integrate f a' b' (n * 2)
                diff = abs (next - current)
            in if diff < e
               then (next, n * 2)
               else refine (n * 2)

-- Паралельна інтеграція з фіксованою кількістю кроків
parIntegrate :: Int -> (Double -> Double) -> Double -> Double -> Int -> Double
parIntegrate k func a b n =
    let chunkSize = (b - a) / fromIntegral k
        nPerChunk = n `div` k
        intervals = [(a + fromIntegral i * chunkSize, a + fromIntegral (i+1) * chunkSize)
                    | i <- [0..k-1]]
        tasks = [integrate func ai bi nPerChunk | (ai, bi) <- intervals]
        parallelResults = tasks `using` parList rdeepseq
    in sum parallelResults