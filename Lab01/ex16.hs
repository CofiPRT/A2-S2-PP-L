ex16 [] = []
ex16 l = if head(l) == True then 1:ex16(tail(l)) else 0:ex16(tail(l))