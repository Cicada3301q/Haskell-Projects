module A3 where 

--Author: Quenten Welch 30054505
--Date: october 24th 2020


--Question 1a. 
data Formula = Var String |
			   Not Formula |
			   And Formula Formula |
			   Or Formula Formula 
			  
			  
--Question 1b.
-- Give a Haskell expression that constructs aFormularepresentation of the formula not(not p and not q)  
f = Not (And (Not (Var "p")) (Not (Var "q")))

c = Not (Not (Var "p"))

a = Not(And (Var "p") (Var "q"))

o = Not(Or (Var "p") (Var "q"))

-- Question 1c
-- Function that takes a given Formula and returns it in the form of a string
showFormula :: Formula -> String
showFormula (Var v)       = v
showFormula (Not f1)      = "~" ++ (showFormula f1)
showFormula (And f1 f2)   = "(" ++ (showFormula f1) ++ " & " ++ (showFormula f2) ++ ")"
showFormula (Or f1 f2)    = "(" ++ (showFormula f1) ++ " V " ++ (showFormula f2) ++ ")"


-- Question 1d
-- Function that eliminates rewrites formulas in NNF 
rewrite :: Formula -> Formula
rewrite (Var v)          = Var v
rewrite (Not (Not (Var v)))   = rewrite(Var v)
rewrite (Not(Var v)) = (Not(Var v))
rewrite (And f1 f2) = And(rewrite f1)(rewrite f2)
rewrite (Or f1 f2) = Or(rewrite f1)(rewrite f2)
rewrite	(Not(And f1 f2)) =  rewrite(Or(Not (rewrite f1)) (Not(rewrite f2)))
rewrite	(Not(Or f1 f2)) =  rewrite(And(Not (rewrite f1)) (Not(rewrite f2)))


--Question 3a
--Function that returns the last element of a list
lastElm :: [a] -> a
lastElm = foldr1 (\_ a -> a)


--helper function for 3b
peel :: [a -> Bool] -> a -> [Bool]
peel [] _ = [True] 
peel (f:fs) x =  ((f x) : (peel fs x))

--Question 3b
unanimous :: [a -> Bool] -> a -> Bool 
unanimous [] _ = True 
unanimous x z = foldr (&&) True (peel x z)


--Question 3c
selectiveMap :: (a->Bool) -> (a->b) -> [a] -> [b]
selectiveMap x y z = map y (filter x z)


--Question 4a 
-- Given a positive integer n, this function computes
-- f (f (f ... (f x))) where f occurs n times. 
iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter 1 f x = f x
iter n f x = f (iter (n-1) f x)


--helper function for 4b, the textbook said to use something called double? idk im assuming it just wants to pass a function to powerOfTwo
double :: Integer -> Integer
double n = 2*n 
--Question 4b
-- using 
powerOfTwo :: Integer -> Integer
powerOfTwo 0 = 1
powerOfTwo n = iter (n-1) double 2
