ex12 :: (Integral a) => [a] -> Bool
ex12 l = if ((((reverse l) !! 2) `mod` 2) == 1) then True else False