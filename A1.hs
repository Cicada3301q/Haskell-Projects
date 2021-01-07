module A1 where 
-- Author: Quenten Welch 30054505
--Date: Oct 2nd

--Question 1
-- This function returns the floor of the log that is produced when taking the log of arg 2 base arg 1
-- implemented by returning the lowest power that exceeds 'y' - 1
myLog :: Integer -> Integer -> Integer

myLog x y 
	| x > y = 0
	| x == y = 1
	| otherwise = head([i | i<-[2..y], x^i > y]) -1
	
--Question 2
	
type Person = String
type Book = String
type Database = [(Person, [Book])]


--returns a list of books loaned by a person
books       :: Database -> Person -> [Book]
books db per = concat [b  | (p, b) <- db, p == per]

--utility function to see if a person has a specified book
hasBook :: [Book] -> Book -> Bool
hasBook [] b = False
hasBook (x:xs) b = (x == b) || (hasBook xs b)

-- returns list of people who have a book on loan
borrowers   :: Database -> Book -> [Person]
borrowers   db bk = [p | (p, b) <- db, hasBook b bk]

-- returns a boolean of whether a book is on loan to a specific person or not
borrowed    :: Database -> Book -> Bool
borrowed db bk = (borrowers db bk) /= []

--returns the numver of books loaned to a certain person
numBorrowed :: Database -> Person -> Int
numBorrowed db per	= length (books db per)
--TODO make exception for already borrowed book

-- adds a record of a book being loaned to a person to the database
makeLoan    :: Database -> Person -> Book -> Database
makeLoan [] per bk	= (per, [bk]) :[]
makeLoan (x:xs) per bk
	| (fst x) == per = (per, [bk]) : []
	| otherwise 	 = x : makeLoan xs per bk


--utility function that removes a book from a given list
removeBook :: [Book] -> Book -> [Book]
removeBook [] _ = []
removeBook (x:xs) b 
	| x == b = removeBook xs b
	| otherwise = x : (removeBook xs b)

-- removes a book loan from a person's loan list
returnLoan  :: Database -> Person -> Book -> Database
returnLoan [] per bk = []
returnLoan (x:xs) per bk
	|(fst x) == per	= (fst x, (removeBook (snd x) bk)) : xs
	| otherwise		= x : returnLoan xs per bk 


-- ////////////////////////////////////////////////////
--Question 3

type Picture = [[Char]]
--Utility function that takes in a list and an int and iterates over the elements of the list and copies them the same number of times as the int
copy :: [a] -> Int -> [a]
copy list itemRepetitions

 =  [ item | item  <- list , repetition <- [1 .. itemRepetitions] ]
   
-- function that uses copy to duplicate the rows as many times as the int argument dictates
scale :: Picture -> Int -> Picture
scale picture int

 |  int < 1    = [[]]
 |  otherwise  = copy [ copy row int | row <- picture ] int


--Question 4

type Graph = [(Int, Int)]

-- function that takes in a graph consiting of 2-tuples and 2 integers as arguments and returns common elements shared by both integers within the graph

commonFriends :: Graph -> Int -> Int -> [Int]
ff g p = [z | (z,w)<-g, w==p]
lf g p = [z | (w,z)<-g, w==p, not(z `elem` ff g p)]
commonFriends g p q = [z | z <- (ff g p)++(lf g p), z`elem`((ff g q)++(lf g q))]

gr :: Graph 
gr = [(1,3), (4, 2), (4, 1), (2, 3)]





