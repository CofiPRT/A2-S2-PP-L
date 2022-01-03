ex13 [] = 0
ex13 l = head(l) + ex13(tail(l))