module A2 where


--Question 1a, 

--takes two lists of integers and adds the integers bases on common index
type Poly = [Integer]
 
addpoly :: Poly -> Poly -> Poly
addpoly [] [] = []
addpoly [] (z:zs) = (z:zs)
addpoly (x:xs) [] = (x:xs)
addpoly (z:zs) (x:xs) = x+z : addpoly zs xs  

--Question 1b, 
--a function that recursively multiplies the elements from Poly1 through each element in poly2
--adding a pad zero after every iteration to ensure elements are stored with their proper degree
--before recursvely adding the lists up according to their degree.  

multpoly :: Poly -> Poly -> Poly
multpoly [] []     = [0]
multpoly (x:xs) [] = [0]
multpoly [] (y:ys) = [0] 
multpoly (xs) (ys) =  addpoly (map (*(head xs)) ys) (0 : (multpoly (tail xs) ys))


--Question 2a


--a function that takes two lists and combines them into one sorted list, 
mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists [] [] = []
mergeLists  (x:xs) [] = (x:xs)
mergeLists [] (y:ys) = (y:ys)
mergeLists (x:xs) (y:ys) 
	| x <= y = x:mergeLists xs (y:ys)	
	| otherwise = y : mergeLists (x:xs) ys



--Question 2b.


--helper function that produces a list of all the number of even index of the input list
getEvens :: [Integer] -> [Integer]
getEvens (x:xs) = x:getOdds xs
getEvens _ = []

--helper function that produces a list of all the number of odd index of the input list
getOdds :: [Integer] -> [Integer]
getOdds (_:xs) = getEvens xs
getOdds _ = []

--function that splits a list into a list of its even and odd indexed components
splitList :: [Integer] -> ([Integer], [Integer])
splitList [] = ([], [])
splitList (x:xs) = (getEvens(x:xs), getOdds(x:xs))


--Question 2c
--function that calls on mergelists and split lists to perform a merge sort
mSort :: [Integer] -> [Integer]
mSort [] = []
mSort [x] = [x]
mSort xs = mergeLists (mSort (fst (splitList xs))) (mSort (snd (splitList xs )))






