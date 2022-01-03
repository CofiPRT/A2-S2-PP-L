ex17 :: [[Integer]] -> [Bool]
ex17 [] = []
ex17 l = (g (head l)):(ex17 (tail l))
    where
        g [] = True
        g l = h (tail l)
        h [] = True
        h l = False