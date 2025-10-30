module Integration.Sequential
    ( integrate
    ) where

-- Розраховує інтеграл на одному ядрі (послідовно)
-- Використовуємо простий метод прямокутників (midpoint rule)
integrate :: (Double -> Double) -> Double -> Double -> Int -> Double
integrate func a b n =
    let
        -- Ширина одного кроку (одного прямокутника)
        step = (b - a) / fromIntegral n
        
        -- Створюємо список точок x_i (центри прямокутників)
        -- [a + 0.5*step, a + 1.5*step, ..., b - 0.5*step]
        points = [a + step * (fromIntegral i + 0.5) | i <- [0..n-1]]
        
        -- Обчислюємо значення f(x_i) для кожної точки
        -- Через лінивість Haskell, обчислення тут ще НЕ відбуваються
        values = map func points
    in
        -- Сумуємо всі значення f(x_i) і множимо на ширину кроку
        -- Обчислення відбудуться "на вимогу", коли ми попросимо 'sum'
        sum values * step