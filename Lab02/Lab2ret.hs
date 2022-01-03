-- functie duplica elemente
ex1 :: [a] -> [a]
ex1 [] = []
ex1 (x:xs) = x:(x:(ex1 xs))

-- multiplica el unei liste de k ori
ex2 :: Int -> [b] -> [b]
ex2 k [] = []
ex2 k (x:xs) = (g k x) ++ (ex2 k xs)
	where
		g 0 x = []
		g k x = x:(g (k-1) x)

myFilter f [] i = []
myFilter f (x:xs) i = if (f i) then x:(myFilter f xs (i+1)) else (myFilter f xs (i+1))

--sterge el dintr-o lista dp pozitiile care sunt divizori ai lui k
ex3 k l = myFilter ((\k i -> if i == 0 then True else (mod k i /= 0)) k) l 0

-- afiseaza elem dintr-un interval [a,b]
ex4 a b l = myFilter ((\a b i -> (i >= a) && (i <= b)) a b) l 0

-- ex5 "323CB" [("323CB", ["Voicu", "Mihai"])] -> 
ex5 g ((a,b):xs) = if (g == a) then b:(ex5 g xs) else (ex5 g xs)