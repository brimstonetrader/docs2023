-- I'm sick of typing matrices into wolfram alpha this code squares a 100 by 100 matrix in a few seconds.


> (#) :: [[Int]] -> [[Int]] -> [Int]
> a # b = mm a b

-- ghci> [[1,2],[0,1]] # [[1,-1],[3,2]]
--   [7,3,3,2]

-- so then [[1,2],   [[1,-1],
--          [0,1]] #  [3, 2]]
--                 
--       [[7,3],
--        [3,2]] = 
--       [[1*1 + 2*3, 1*-1 + 2*2], 
--        [0*1 + 1*3, 0*-1 + 1*2]]

--  i would need to work harder to make it work on non squares but oy 

> choo


> rm :: Int -> Int -> Int -> Int -> [[Int]]
> rm seed 0 c m = []
> rm seed r c m = (rng seed c m):(rm ((m-r)*seed) (r-1) c m)

> mm :: [[Int]] -> [[Int]] -> [Int]
> mm xss yss = do
>   if len yss == len (xss !! 0) 
>     then mm2 0 (len xss) (len (yss !! 0)) (len yss) xss (transpose yss) 
>     else []

> mm2 :: Int -> Int -> Int -> Int -> [[Int]] -> [[Int]] -> [Int]
> mm2 i r c f xss yss = if i == r*c then [] else do
>   let (a,b) = i /%/ r 
>   (dot (xss !! a) (yss !! b)):(mm2 (i+1) r c f xss yss)

> dot :: [Int] -> [Int] -> Int
> dot [] [] = 0
> dot (x:xs) (y:ys) = (x*y)+(dot xs ys)

> transpose :: [[Int]] -> [[Int]]
> transpose mx = init (split (len mx) (transpose2 0 (len mx) mx))

> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b


> split :: Int -> [Int] -> [[Int]]
> split i is = if i>(len is) then [is] else do
>   let (as,bs) = splitAt (fromIntegral i) is
>   as:(split i bs)

> transpose2 :: Int -> Int -> [[Int]] -> [Int]
> transpose2 i n os = if i==(n*n) then [] else 
>   ((os !! (i `mod` n)) !! (i `div` n)):(transpose2 (i+1) n os)

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b



> refl :: Int -> Int -> [Int] -> [Int]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i%n)) + (i//n)))):(refl (i+1) n os)

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> rng :: Int -> Int -> Int -> [Int]
> rng ornd 1  c = [(inst ornd)%c]
> rng ornd it c = do 
>   let nrnd = inst ornd 
>   ((nrnd % c): rng nrnd (it-1) c)