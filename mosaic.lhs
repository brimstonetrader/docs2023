
----------------------------------------------------------------
--
-- Mosaic
--

-- Random Number Generator
--
-- Standard
--
--  Chapter 7.1, Eq. 7.1.6
--  parameters from Knuth and H. W. Lewis

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

-- 1/5th is what Tatham uses. Let's allow for user variance.

> lcg2 :: Int -> Int -> [Int]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Int -> [Int] -> [Int]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Int] -> [Int] -> [Int] -> [Int]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> noiseGrid :: Int -> Int -> Int -> [Int]
> noiseGrid a b seed = permute seed ((amanybs a 1) ++ (amanybs (b-a) 0))



-- Now with one keyboard smash, we can generate a 
-- random noise grid with a particular amount of 
-- ones.

ghci> noiseGrid 8 64 98393
	[0,0,0,1,0,0,0,0,
	 0,0,0,1,0,0,0,0,
	 0,0,0,0,0,0,0,0,
	 1,0,0,0,1,1,0,1,
	 0,0,0,0,0,0,1,0,
	 0,0,0,0,0,0,0,0,
	 0,0,0,1,0,0,0,0,
	 0,0,0,0,0,0,0,0]

-- Reflects a square list about the \ line, like

   111    100
   001 -> 100
   001    111

-- turning rows to columns, and vice versa.

> refl :: Int -> Int -> [Int] -> [Int]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i `mod` n)) + (i `div` n)))):(refl (i+1) n os)

-- ghci> (refl 0 4 
				[1,1,0,0,
				 0,1,0,0,
				 0,1,1,0,
				 0,0,0,1])
				 
					->
					
				[1,0,0,0,
				 1,1,1,0,
				 0,0,1,0,
				 0,0,0,1]




-- Determines count of both rows and columns of a square list. Outputs
-- ((rows, columns), square) like
-- 
	 2 2 2 2
   3 X X O X
   2 X O X O -> (([3,2,3,1],[2,2,2,2]),[1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0])
   3 O X X X
   1 O X O O


-- We will give a particular index (n*y + x), and we want the sum of all
-- neighbors. So 

-- 011
-- 1X1 -> getNeighborCount X = 6
-- 101

> getNeighborCount :: [Int] -> Int -> Int 
> getNeighborCount gs i = do 
>   let g = sq (len gs)
>   let top = if i >= g         then True else False 
>   let lft = if i % g > 0      then True else False 
>   let rit = if i % g < g-1    then True else False 
>   let btm = if g*g - i > g    then True else False 
>   let st = show(top, lft, rit, btm)
>   let n = if top         then gs !! (i-g)   else 0
>   let nw = if top && lft then gs !! (i-g-1) else 0
>   let w = if lft         then gs !! (i-1)   else 0
>   let sw = if btm && lft then gs !! (i+g-1) else 0
>   let s = if btm         then gs !! (i+g)   else 0
>   let se = if btm && rit then gs !! (i+g+1) else 0
>   let e = if rit         then gs !! (i+1)   else 0
>   let ne = if top && rit then gs !! (i-g+1) else 0
>   n + s + e + w + nw + sw + ne + se + (gs !! i)

> mosaic :: Int -> Int -> IO ()
> mosaic seed n = do 
>   let grid  = (noiseGrid (0*((     (seed) % 6 - 3) + (n*n // 2))+(n*n)) (n*n)       seed)
>   let hlocs = (noiseGrid (0*(((inst seed) % 6 - 3) + (n*n // 2))+(n*n)) (n*n) (inst seed))
>   let hints = (findHints 0 grid hlocs)
>   putStrLn (replicate (2*n+1) '_')
>   printPuzzle hints n

> printPuzzle :: [Int] -> Int -> IO ()
> printPuzzle [] n = putStr ""
> printPuzzle hints n = do 
>   let (a,b) = splitAt n hints
>   let s = '|':(toStr a)
>   putStrLn s
>   printPuzzle b n

> toStr :: [Int] -> String
> toStr []      = ""
> toStr (10:zs) = '_':'|':(toStr zs)
> toStr (z:zs) = (intToChar z):'|':(toStr zs)

> findHints :: Int -> [Int] -> [Int] -> [Int]
> findHints i grid []        = []
> findHints i grid (l:hlocs) = (if l==1 then (getNeighborCount grid i) else 10):(findHints (i+1) grid hlocs)

-------------------------------------------------------------------------

-- Not worth explaining

> intToChar :: Int -> Char
> intToChar 1  = '1'
> intToChar 2  = '2'
> intToChar 3  = '3'
> intToChar 4  = '4'
> intToChar 5  = '5'
> intToChar 6  = '6'
> intToChar 7  = '7'
> intToChar 8  = '8'
> intToChar 9  = '9'
> intToChar x  = intToChar (x`mod`16)

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y


> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)