ex14 :: [Bool] -> Bool
ex14 [] = True
ex14 l = head(l) && ex14(tail(l))