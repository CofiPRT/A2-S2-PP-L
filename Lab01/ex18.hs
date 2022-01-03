ex18 :: [Bool] -> Int
ex18 [] = 0
ex18 l = if head(l) == True then 1 + ex18(tail(l)) else ex18(tail(l))