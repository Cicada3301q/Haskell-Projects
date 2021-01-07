
module A4 where
--Author: Quenten Welch 30054505
--Date: November 20 20202

-- Question 1
-- Poly data typle for Q1
data Poly = PConst Int |
            PVar |
            PAdd Poly Poly |
            PMul Poly Poly

-- Denotational compiler for the Poly data type
compilePoly :: Poly -> (Int -> Int)
compilePoly (PConst n)   = (\x -> n)
compilePoly  PVar        = (\x -> x)
compilePoly (PAdd p1 p2) = (\x -> (compilePoly p1) x + (compilePoly p2) x)
compilePoly (PMul p1 p2) = (\x -> (compilePoly p1) x * (compilePoly p2) x)

-- Question 4
-- Calculates the running sum of a list of Ints
runningSums :: [Int] -> [Int]
runningSums = scanl (+) 0
