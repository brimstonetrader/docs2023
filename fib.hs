> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

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