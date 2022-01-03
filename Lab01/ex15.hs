ex15 [] = []
ex15 l = if (head(l) `mod` 2) == 1 then head(l):ex15(tail(l)) else []++ex15(tail(l))