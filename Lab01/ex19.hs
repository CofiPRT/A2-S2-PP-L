ex19 :: [Int] -> [Int]
ex19 [] = []
ex19 l = insert (head l) (ex19 (tail l))
    where
        insert :: Int -> [Int] -> [Int]
        insert e [] = [e]
        insert e l = if e <= (head l) then e:l else (head l):(insert e (tail l))