 inst :: Int -> Int
 inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

 rng :: Int -> Int -> Int -> [Int]
 rng ornd 1  cap = [(inst ornd) % cap]
 rng ornd it cap = do 
   let nrnd = inst ornd 
   (nrnd % cap : rng nrnd (it-1))

-- Has disappointingly small modular periods, but
-- an Int division of 99 seems to clean it up.

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = (((a `div` 79) `mod` b)+1):(listMod as b)



-- Now with one keyboard smash, we can generate a full 
-- random noise grid.

-- ghci> (listMod (rng 687628376 100) 2)
   [1,1,0,0,0,1,0,1,0,0,
	1,0,0,1,0,1,1,0,0,1,
	1,0,1,1,0,1,1,0,1,1,
	0,1,0,0,0,0,0,1,0,1,
	1,1,0,0,0,0,1,0,1,0,
	1,1,0,1,1,0,0,1,1,1,
	1,1,1,0,1,1,0,1,0,1,
	0,1,0,1,0,1,1,1,1,1,
	0,1,1,1,0,0,1,1,0,1,
	1,0,0,1,1,1,0,1,1,1]




	
-- The minimal excluded element is the smallest that's not there.
-- mex(1,5,7,2,3) = 4

mexOfN :: Int -> Int -> Double
mexOfN seed n = do 
let rnds = (listMod (rng seed (n^3)) n)
avg (mexEach n rnds)


> mex :: [Int] -> Int -> Double
> mex as m = if m `elem` as then (mex as (m+1)) else realToFrac m

> avg :: [Double] -> Double
> avg ds = (sum' ds) / (realToFrac (len ds))

> sum' :: [Double] -> Double
> sum' []     = 0.0
> sum' (d:ds) = d+(sum' ds)

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)



> mN :: Int -> Double 
> mN n = avg (mexEach2 n (allPossibleSequences n) )

> rep :: Int -> Int -> [Int]
> rep x 0 = []
> rep x n = x:(rep x (n-1))

> allPossibleSequences :: Int -> [[Int]]
> allPossibleSequences n = aPS n 1 (n^n) 

> aPS :: Int -> Int -> Int -> [[Int]]
> aPS n a b  = if a>b then [] else (expand a 0 n):(aPS n (a+1) b)

> expand :: Int -> Int -> Int -> [Int]
> expand a i n = if i==n then [] else (a%n+1):(expand (a//n) (i+1) n)

> mexEach2 :: Int -> [[Int]] -> [Double]
> mexEach2 _ [] = []
> mexEach2 n (ss:sss) = (mex ss 1):(mexEach2 n sss)


> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b
