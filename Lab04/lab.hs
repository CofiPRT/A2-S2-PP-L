type Matrix = [[Integer]]
type Image = [String]

m1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "
 
logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]

overlay1 = ["******", "**  **", "**  **", "******"]
overlay2 = [" **** ", "******", " **** ", "  **  "]

-- 1
parsem :: String -> Matrix
parsem = (map ((map rd).(splitBy ' '))).(splitBy '\n')
	where
		splitBy :: Char -> String -> [String]
		splitBy c = foldr op []
			where
				op x[]
					| x /= c = [[x]]
					| otherwise = [[]]
				op x (y:ys)
					| x /= c = (x:y):ys
					| otherwise = []:(y:ys)
		rd x = read x :: Integer

-- 2
toString :: Matrix -> String
-- toString mat -- foldl f [] mat
-- 	where
-- 		f x y -- x ++ (foldl g )
toString = (foldr (\x acc -> x ++ "\n" ++ acc) []) . (map (foldr (\x acc -> (show x) ++ " " ++ acc) []))

-- 3
displaymat = putStrLn . toString

-- 4
vprod :: Integer -> Matrix -> Matrix
vprod k mat = map (map (\x -> x * k)) mat

-- 5
hjoin :: [[a]] -> [[a]] -> [[a]]
hjoin mat1 mat2 = zipWith (++) mat1 mat2

-- 6
vjoin :: [[a]] -> [[a]] -> [[a]]
vjoin mat1 mat2 = mat1 ++ mat2

-- 7
msum :: Matrix -> Matrix -> Matrix
msum mat1 mat2 = zipWith (zipWith (+)) mat1 mat2

-- 8
tr :: [[a]] -> [[a]]
tr ([]:_) = []
tr mat = (map head mat) : tr (map tail mat)

-- 9
mprod :: Matrix -> Matrix -> Matrix
mprod mat1 mat2 = map (f (tr mat2)) mat1
    where
        f fmat2 row1 = map (g row1) fmat2
        g row1 row2 = sum (zipWith (*) row1 row2)

-- 10
toStringImg :: Image -> String
toStringImg img = foldl (++) [] img

displaym = putStrLn . toStringImg

-- 11
flipH :: Image -> Image
flipH img = reverse img --tr (map reverse (tr img))

-- 12
flipV :: Image -> Image
flipV img = map reverse img

-- 13
rotate90r :: Image -> Image
rotate90r img = map (reverse) (tr img)

-- 14
rotate90l :: Image -> Image
rotate90l img = reverse(tr img)

-- 15
diamond :: Integer -> Image
diamond h = f (2*h-1) (2*h-1)
	where
		f 0 k = []
		f x k = ((replSp x k) ++ (replSt x k) ++ (replSp x k)):(f (x-1) k)
		replSp xSp kSp = replicate (fromIntegral (div (kSp - (g xSp kSp)) 2)) ' '
		replSt xSt kSt = replicate (fromIntegral (g xSt kSt)) '*'
		g x' k' = k' - abs(2*x' - k' - 1)
		
-- 16
overlay :: Image -> Image -> Image
overlay img1 img2 = zipWith (zipWith f) img1 img2
	where
		f x y = if (x == '*' && y == '*') then '*' else ' '

-- 17
upperTriang :: Matrix -> Matrix
upperTriang mat = map ()

-- helper functions
display :: (Show a) => ([a] -> String) -> [[a]] -> IO ()
display displayLine = putStr . foldr (++) "" . map displayLine

displayImg :: [String] -> IO ()
displayImg = display (++ "\n")