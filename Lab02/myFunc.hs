group [] = []
group (x:xs) = (x:a) : group b
	where
		(a, b) = span ((==) x) xs

getTuple [] = []
getTuple (x:xs) = (show (length x), [(head x)]) : (getTuple xs)