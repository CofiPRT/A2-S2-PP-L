module MyReverse where

ex11 [] = []
ex11 l = (ex11 (tail l)) ++ [head l]