import Data.List

-- 1. Consider the following datatype which encodes λ-expressions:
-- 7. Add two data constructors to the type LExpr so that we can also model functions and applications in uncurry form. Examples: (λx y z.<body>), (f x y z).
data LExpr = Var Char 
            | Lambda Char LExpr
            | App LExpr LExpr 
            | Lambda2 [Char] LExpr
            | App2 [LExpr]


-- 8. Write a proper display function for these new constructors.
instance Show LExpr where
        show (Var c) = [c]
        show (Lambda c l) = "λ" ++ [c] ++ "." ++ show l 
        show (Lambda2 variables l) = "λ" ++ variables ++ "." ++ show l 
        show (App l1 l2) = "(" ++ (show l1) ++ " " ++ (show l2) ++ ")"
        show (App2 l1) = "(" ++ (showlist l1) ++ ")"

showlist :: [LExpr] -> [Char]
showlist (x : []) = (show x)
showlist (x : xs) = (show x) ++ " " ++ (showlist xs) 


--  2. Write a function vars which returns a list of variables used in a λ-expression:
vars :: LExpr -> [Char]
vars (Var c) = [c]
vars (Lambda c l) = rmdups ([c] ++ (vars l) )
vars (App l1 l2) = rmdups ((vars l1) ++ (vars l2)) 


rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = if elem x xs
                then rmdups xs
                else x : rmdups xs


-- 3. Write a function reducible which tests if an expression can be reduced to another. Write tests first! What are the cases when an expression is reducible? 
reducible :: LExpr -> Bool
reducible (App (Lambda c l) (Var c2)) = True
reducible (App (Lambda c l) (Lambda c2 l2)) = if c2 == c
                                                then False
                                                else True
reducible _ = False


-- 4. Write a function which renames all occurrences of a variable with another, in a λ-expression:
-- 6. Write a function which takes a λ-expression of the form (λx.<body> <arg>) and reduces it in a SINGLE step. 
rename :: Char -> Char -> LExpr -> LExpr
rename c1 c2 (Var c) = if c == c1
                        then Var c2
                        else Var c

rename c1 c2 (Lambda c l) = if c == c1
                            then Lambda c2 (rename c1 c2 l)
                            else Lambda c (rename c1 c2 l)

rename c1 c2 (App l1 l2) = App (rename c1 c2 l1) (rename c1 c2 l2)


-- 5. Write a function which replaces all occurrences of a variable with a λ-expression, in a λ-expression: 
replace :: Char -> LExpr -> LExpr -> LExpr
replace c (Lambda c2 l)  (Var c3) = if c == c2
                                    then rename c2 c3 (Lambda c2 l)
                                    else Lambda c2 l    -- \x.x y -> \y.y  

replace c (Lambda c2 l)  (Lambda c3 l2) = if c == c2
                                            then rename c2 c3 (replace c l (Lambda c3 (Lambda c3 l2)))
                                            else Lambda c2 l -- \x.x \y.y -> \y.\y.y

replace c (Lambda c2 l)  (App l1 l2) = if c == c2
                                        then Lambda c2 (replace c l (App l1 l2))
                                        else Lambda c2 l   -- \x.x (x x) -> \x.(x x)

replace c (App l1 l2) (Lambda c2 l3) = App (Lambda c2 l3) (Lambda c2 l3) --de aici in jos sunt apelate doar pentru a termina recursia din lambdas

replace c (App l1 l2) (Var c2) = App (replace c l1 (Var c2)) (replace c l2 (Var c2))

replace c (Var c2) l = if c == c2
                        then l
                        else Var c2


-- 6. SEE 4.

-- 7. SEE 1.

-- 8. SEE 1.

-- 9. Write a function luncurry which takes an uncurries λ-expression and transforms it in curry form.
luncurry :: LExpr -> LExpr
luncurry (App l1 (App2 [])) = l1

luncurry (App l (App2 (x:xs))) =luncurry (App (App l x) (App2 xs))

luncurry (App2 (x:xs)) = luncurry (App x (App2 xs))

luncurry (Lambda2 [] lex2) = lex2

luncurry (Lambda2 (x:xs) lex2) = Lambda x (luncurry (Lambda2 xs lex2))


-- 10. Write a function lcurry which takes a curried λ-expression and transforms it in uncurry form. 
lcurry :: LExpr -> LExpr
lcurry (Var c) = (Var c)

lcurry (Lambda c (Lambda c2 lex2)) = lcurry (Lambda2 ([c] ++ [c2]) lex2)

lcurry (Lambda c lex2) = (Lambda c lex2)

lcurry (Lambda2 str (Lambda c lex2)) = lcurry (Lambda2 (str ++ [c]) lex2)

lcurry (Lambda2 str lex2) = Lambda2 str (lcurry lex2) -- daca nu mai intra pe prima inseamnca ca nu mai trebuie sa facem curry pe lambda dar poate mai merge 

lcurry (App (App2 l1) l2) = lcurry (App2 (l1++[lcurry l2]))

lcurry (App l1 (App2 l2)) = lcurry (App2 ([lcurry l1] ++ l2))

lcurry (App l1 l2) = lcurry (App2 ([lcurry l1] ++ [lcurry l2]))

lcurry (App2 ((App2 l) : x)) = lcurry (App2 (l ++ x))

lcurry (App2 l) = App2 l


-- 11. Write the function fv which computes the list of all free variables of a λ-expression. 
fv :: LExpr -> [Char]
fv (App (Var c) l2) = [c] ++ (fv l2)
fv (App (Lambda c lexpr) (Var c2)) = (fv lexpr) ++ [c2]
fv (Lambda c lexpr) = rmdups (freeLambda  (Lambda c lexpr) [])
fv (App l1 l2) = (fv l1) ++ (fv l2)
fv (Var c) = []

freeLambda :: LExpr -> [Char] -> [Char]
freeLambda (Lambda c1 l) lista_lambde = freeLambda l (lista_lambde ++ [c1])  
freeLambda (Var c) lista_lambde = if verifyBounded c lista_lambde
                                    then [c]
                                    else []

freeLambda (App l1 l2) lista_lambde = (freeLambda l1 lista_lambde) ++ (freeLambda l2 lista_lambde)

verifyBounded :: Char -> [Char] -> Bool
verifyBounded c [] = True
verifyBounded c (x:xs) = if c==x
                            then False
                            else verifyBounded c xs


-- 12. Write a function bv which computes the list of all bound variables of a λ-expression. 
bv :: LExpr -> [Char]
bv l = eliminateFree (allVariables l) (fv l)

allVariables :: LExpr -> [Char]
allVariables (Var c) = [c]
allVariables (Lambda c l) = allVariables l
allVariables (App l1 l2) = (allVariables l1) ++ (allVariables l2)

eliminateFree :: [Char] -> [Char] -> [Char]
eliminateFree all [] = all
eliminateFree all (f : free) = eliminateFree (filter (/=f) all) free


-- 13. Write a function subst which computes the textual substitution of all free occurrences of some variable x by e in e', according to the lecture definition: 
subst :: Char -> LExpr -> LExpr -> LExpr
subst c e (App l1 l2) = if not $ verifyBounded c (fv (App l1 l2))
                        then App (subst c e l1) (subst c e l2)
                        else App l1 l2

subst c e (Lambda c2 l) = if not (verifyBounded c (fv (Lambda c2 l)))
                            then Lambda c2 (subst c e l)
                            else Lambda c2 l

subst c e (Var c2) = if c==c2
                        then e
                        else Var c2


-- 14. Implement a function which reduces a reducible λ-expression to an irreducible one. (According to the lecture definition, what happens with λx.(λx.x x) ?
return_expression (Lambda c l) = l

reduction :: LExpr -> LExpr
reduction (App (Lambda c l) l2) = return_expression (replace c (Lambda c l) l2)

reduction (App (App (Lambda c l) l2) l3) = reduction (App (return_expression (replace c (Lambda c l) l2)) l3) 

reduction (App (Var c) l) = App (Var c) (reduction l)

reduction (Lambda c lex2) = if not (verifyBounded c (fv lex2))
                            then Lambda 'ε' (reduction lex2)
                            else Lambda c (reduction lex2)

reduction l = l


-- 15. Implement normal-order evaluation. 
normal_eval :: LExpr -> LExpr
normal_eval (App (Lambda 'x' lex1) lex2) = normal_eval (subst 'x' lex2 lex1)    --red

normal_eval (App e e') = if reducible e
                            then normal_eval (App (reduction e) e')
                            else App e e'   --app1

normal_eval lex = lex


-- 16. Implement applicative (strict) evaluation. 
applicative_eval :: LExpr -> LExpr
applicative_eval (App (Lambda 'x' lex1) lex2) = applicative_eval (subst 'x' lex2 lex1)  --red

applicative_eval (App e e') = if (reducible e) || (reducible e')
                                then (if reducible e'
                                        then App e (reduction e')
                                        else  App (reduction e) e')
                                else App e e'  --if reducible e' app2 else app1

applicative_eval lex = lex

-- cmFyZXNnc21hcnRAZ21haWwuY29t
-- bGZsZ3RsbnRobHprbHNybg==
