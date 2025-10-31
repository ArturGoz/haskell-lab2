module Integration.Sequential
    ( integrateEpsilon
    , integrate
    ) where

-- Інтеграція з заданою точністю epsilon
integrateEpsilon :: (Double -> Double) -> Double -> Double -> Double -> (Double, Int)
integrateEpsilon func a b eps = refine 100
  where
    refine n =
        let current = integrate func a b n
            next = integrate func a b (n * 2)
            diff = abs (next - current)
        in if diff < eps
           then (next, n * 2)
           else refine (n * 2)

-- Базова інтеграція з фіксованою кількістю кроків
integrate :: (Double -> Double) -> Double -> Double -> Int -> Double
integrate func a b n =
    let step = (b - a) / fromIntegral n
        points = [a + step * (fromIntegral i + 0.5) | i <- [0..n-1]]
        values = map func points
    in sum values * step