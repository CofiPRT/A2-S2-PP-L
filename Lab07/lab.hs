{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
import Data.Typeable

-- Ex 1
data Extended = Infinity | 
                Value Integer

instance Eq Extended where
    Infinity == Infinity = True
    Value x  == Value y  = (x == y)
    _        == _        = False
    Infinity /= Infinity = False
    Value x  /= Value y  = (x /= y)
    _        /= _        = True
    

-- Ex 2
data Formula a = Atom a |
                 Or (Formula a) (Formula a) |
                 And (Formula a) (Formula a) |
                 Not (Formula a)

class Bconvert a where
    convert :: a -> Bool

instance Bconvert String where
    convert x
        | x == "x"  = True
        | otherwise = False

instance Bconvert Integer where
    convert x
        | x == 1    = True
        | otherwise = False

instance Bconvert Bool where
    convert x = x

eval_formula :: (Bconvert a) => (Formula a) -> Bool
eval_formula (Atom x) = (convert x)
eval_formula (Or f1 f2) = (eval_formula f1) || (eval_formula f2)
eval_formula (And f1 f2) = (eval_formula f1) && (eval_formula f2)
eval_formula (Not f) = not(eval_formula f)

instance (Bconvert a, Eq a) => Eq (Formula a) where
    (Atom x) == (Atom y) = (x == y)
    f1 == f2 = (eval_formula f1) == (eval_formula f2) 
    (Atom x) /= (Atom y) = (x /= y)
    f1 /= f2 = (eval_formula f1) /= (eval_formula f2)

-- Ex 3
data Set a = F (a->Bool)

eval_set :: a -> (Set a) -> Bool
eval_set x (F f) = (f x)

union f g = (\x -> (f x) || (g x))
inters f g = (\x -> (f x) && (g x))
notSet f = (\x -> not(f x))

instance (Num a, Typeable a) => Num (Set a) where
    (F s1) + (F s2) = F (union s1 s2)
    (F s1) * (F s2) = F (inters s1 s2)
    fromInteger x   = F isInteger
    negate (F s) = F (notSet s)
    abs (F s) = F s
    signum (F s) = F s

instance Show (Set a) where
    show (F isInteger) = "(F" ++ ", IntegerFunc)"

fromIntegerToSet :: Integer -> (Set Integer)
fromIntegerToSet x = fromInteger x

isType :: (Typeable a) => a -> TypeRep -> Bool
isType x t = (typeOf x) == t

isInteger :: (Typeable a) => a -> Bool
isInteger x = (typeOf x) == (typeOf(toInteger 1))

-- Ex 4
type Var = String
type Dict = [(String, Integer)]
test_dict = [("a", 1), ("b", 2), ("c", 3), ("d", 4)]

class Eval a b | a -> b where
    eval :: Dict -> a -> b

data PExpr = Val Integer |
             Var String  |
             PExpr :+: PExpr 

data BExpr = PExpr :==: PExpr | 
             PExpr :<: PExpr |
             BNot BExpr |
             BExpr :&&: BExpr

data Prog = PlusPlus Var |        -- x++;
            Var :=: PExpr |     -- x = <expr>;
            DeclareInt Var |      -- int x;
            Begin Prog Prog |     -- <p> <p'>
            While BExpr Prog |     -- while (<expr>) { <p> }
            If BExpr Prog Prog      -- if (<expr>) { <p> } else { <p'> }

------------------------------------------------------------------------------- 

ins :: String -> Integer -> Dict -> Dict
ins k val [] = [(k, val)]
ins k val (tpl:xs)
    | (k == (fst tpl)) = ((k, val):xs)
    | otherwise = tpl : (ins k val xs)

valueOf :: String -> Dict -> Integer
valueOf k (tpl:xs)
    | (k == (fst tpl)) = snd tpl
    | otherwise = valueOf k xs

instance Eval PExpr Integer where
    eval [] _                   = 0
    eval dict (Var k)           = (valueOf k dict)
    eval dict (Val i)           = 0
    eval dict (expr1 :+: expr2) = (+) (eval dict expr1) (eval dict expr2)

instance Eval BExpr Bool where
    eval [] _ = True
    eval dict (bexpr1 :&&: bexpr2) = (eval dict bexpr1) && (eval dict bexpr2)
    eval dict (BNot bexpr) = not (eval dict bexpr)
    eval dict (pexpr1 :==: pexpr2) = (eval dict pexpr1) == (eval dict pexpr2)
    eval dict (pexpr1 :<: pexpr2)  = (eval dict pexpr1) < (eval dict pexpr2)

instance Eval Prog Dict where
    eval dict (PlusPlus k) = ins k ((valueOf k dict) + 1) dict
    eval dict (DeclareInt k) = ins k 0 dict
    eval dict (k :=: pexpr) = ins k (eval dict pexpr) dict
