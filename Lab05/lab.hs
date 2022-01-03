import Control.Exception (assert)

-- 1
data IntList = EmptyI | ConsI Integer IntList

iList1 = EmptyI
iList2 = ConsI 5 EmptyI
iList3 = ConsI 1 (ConsI (-5) (ConsI 605 EmptyI))

-- 2
isum :: IntList -> Integer
isum EmptyI = 0
isum (ConsI x xs) = x + (isum xs)

test2 = [
	assert (isum iList1 == 0) "1/3",
	assert (isum iList2 == 5) "2/3",
	assert (isum iList3 == 601) "3/3"
	]

-- 3
data List a = Empty | Cons a (List a)

list1 = to_poly_list iList1
list2 = to_poly_list iList2
list3 = to_poly_list iList3

-- 4
to_poly_list :: IntList -> List Integer
to_poly_list EmptyI = Empty
to_poly_list (ConsI x xs) = Cons x (to_poly_list xs)

-- 5
show_list :: (List a) -> [a]
show_list Empty = []
show_list (Cons x xs) = x : (show_list xs)

test4 = [
	assert (show_list (to_poly_list iList1) == []) "1/3",
	assert (show_list (to_poly_list iList2) == [5]) "2/3",
	assert (show_list (to_poly_list iList3) == [1, -5, 605]) "3/3"
	]

data Tree a = Void | Node (Tree a) a (Tree a) deriving Show

tree1 = Void
tree2 = Node Void 5 Void
tree3 = Node (Node Void 1 Void) 2 (Node (Node Void 3 Void) 4 tree2)

-- 6
flatten :: (Tree a) -> (List a)
flatten Void = Empty
flatten (Node l x r) = Cons x (app (flatten l) (flatten r))

test6 = [
	-- assert (show_list (flatten tree1) == []) "1/3",
	assert (show_list (flatten tree2) == [5]) "1/2",
	assert (show_list (flatten tree3) == [2, 1, 4, 3, 5]) "2/2"
	]

-- 7
app :: (List a) -> (List a) -> (List a)
app Empty Empty = Empty
app Empty ys = ys
app xs Empty = xs
app (Cons x xs) ys = Cons x (app xs ys)

test7 = [
	assert (show_list (app list1 list2) == [5]) "1/3",
	assert (show_list (app list3 list1) == [1, -5, 605]) "2/3",
	assert (show_list (app list2 (app list1 list3)) == [5, 1, -5, 605]) "3/3"
	]

-- 8
tmap :: (a -> b) -> (Tree a) -> (Tree b)
tmap f Void = Void
tmap f (Node l x r) = Node (tmap f l) (f x) (tmap f r)

test8 = [
	assert (show_list (flatten (tmap ((+) 3) tree2)) == [8]) "1/2",
	assert (show_list (flatten (tmap ((*) (-1)) tree3)) == [-2, -1, -4 ,-3, -5]) "2/2"
	]

-- 9
tzipWith :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
tzipWith f (Node l1 x1 r1) (Node l2 x2 r2) = Node (tzipWith f l1 l2) (f x1 x2) (tzipWith f r1 r2)
tzipWith f _ _ = Void

test9 = [
	assert (show_list (flatten (tzipWith (+) tree2 tree2)) == [10]) "1/2",
	assert (show_list (flatten (tzipWith ((-)) tree3 (tmap ((+) 3) tree3))) == [-3, -3, -3, -3, -3]) "2/2"
	]

-- 10
tfoldr :: (a -> b -> b) -> b -> (Tree a) -> b
tfoldr f base Void = base
tfoldr f base (Node l x r) = tfoldr f (f x (tfoldr f base r)) l

test10 = [
	assert (tfoldr (+) 0 tree2 == 5) "1/2",
	assert (tfoldr (+) 0 tree3 == 15) "2/2"
	]

-- 11
tflatten :: (Tree a) -> (List a)
tflatten t = tfoldr Cons Empty t

test11 = [
	assert (show_list (tflatten tree2) == [5]) "1/2",
	assert (show_list (tflatten tree3) == [1, 2, 3, 4, 5]) "2/2"
	]

data Extended = Infinity | Value Integer

-- 12
extSum :: Extended -> Extended -> Extended
extSum (Value x) (Value y) = Value (x + y)
extSum _ _ = Infinity

test12 = [
	assert (equal (extSum Infinity (Value 3)) Infinity) "1/2",
	assert (equal (extSum (Value 5) (Value (-12))) (Value (-7))) "2/2"
	]

-- 13
equal :: Extended -> Extended -> Bool
equal Infinity Infinity = True
equal Infinity _ = False
equal _ Infinity = False
equal (Value x) (Value y) = (x == y)

-- data Maybe a = Nothing | Just a

-- 14
lhead :: (List a) -> Maybe a
lhead Empty = Nothing
lhead (Cons x xs) = Just x

-- 15
ltail :: (List a) -> Maybe (List a)
ltail Empty = Nothing
ltail (Cons x xs) = Just xs