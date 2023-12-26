> import Text.Printf
> import Control.Exception
> import System.CPUTime


> time :: IO t -> IO t
> time a = do
>     start <- getCPUTime
>     v <- a
>     end   <- getCPUTime
>     let diff = (fromIntegral (end - start)) / (10^12)
>     printf "Computation time: %0.3f sec\n" (diff :: Double)
>     return v
> 
> main = do
>     putStrLn "tree recursive fib(28)"
>     time $ fibrec    26 `seq` return ()
>     putStrLn "tail recursive fib(50000)"
>     time $ fibtail   50000 `seq` return ()
>     putStrLn "matrix exponentiation fib(4000000)"
>     time $ fibmatrix 4000000 `seq` return ()
>     putStrLn "Done."

ghci> main
tree recursive fib(28)
Computation time: 0.078 sec
tail recursive fib(50000)
Computation time: 0.062 sec
matrix exponentiation fib(4000000)
Computation time: 0.016 sec
Done.


> fibrec :: Integer -> Integer
> fibrec 0 = 0
> fibrec 1 = 1
> fibrec n = fibrec (n-1) + fibrec (n-2)

> fibtail :: Integer -> Integer
> fibtail n = f n (0, 1)
>          
> f :: Integer -> (Integer, Integer) -> Integer 
> f 0 (a,b) = a
> f n (a,b) = f (n - 1) (b, a + b)
	
> fibmatrix :: Integer -> Integer 
> fibmatrix i = (binarympow [[1,1],[1,0]] i) !! 1 !! 0	
	
	
	
	
	
	
	
	
	
	
	
> binarympow :: [[Integer]] -> Integer -> [[Integer]] 
> binarympow m p = do 
>   let bs = [d+1 | d <- [0..(log2 p)], (p // (2^d)) % 2 == 1]
>   bmp2 bs m [[1,0],[0,1]]

> bmp2 :: [Integer] -> [[Integer]] -> [[Integer]] -> [[Integer]] 
> bmp2 []     im nm = nm
> bmp2 (b:bs) im nm = bmp2 bs im (nm # (mrec im b))

> mrec :: [[Integer]] -> Integer -> [[Integer]]
> mrec m 1 = m
> mrec m i = mrec (m # m) (i-1)

> mpow :: [[Integer]] -> Integer -> [[Integer]]
> mpow m i = mp2 m m i 

> mp2 :: [[Integer]] -> [[Integer]] -> Integer -> [[Integer]]
> mp2 l r 1 = r 
> mp2 l r i = mp2 l (l # r) (i-1)



> log2 :: Integer -> Integer
> log2 1 = 1
> log2 x = 1+(log2 (x // 2))

> (#) :: [[Integer]] -> [[Integer]] -> [[Integer]]
> (#) xss yss = do
>   if len yss == len (xss !! 0) 
>     then rect (len xss) (mm2 0 (len xss) (len (yss !! 0)) (len yss) xss (transpose yss)) 
>     else []

> rect :: Integer -> [Integer] -> [[Integer]]
> rect n []   = []
> rect n xss  = do 
>   let (yss,zss) = splitAt (fromIntegral n) xss
>   yss:(rect n zss)

> mm2 :: Integer -> Integer -> Integer -> Integer -> [[Integer]] -> [[Integer]] -> [Integer]
> mm2 i r c f xss yss = if i == r*c then [] else do
>   let (a,b) = i /%/ r 
>   (dot (xss !! (fromIntegral a)) (yss !! (fromIntegral b))):(mm2 (i+1) r c f xss yss)

> dot :: [Integer] -> [Integer] -> Integer
> dot [] [] = 0
> dot (x:xs) (y:ys) = (x*y)+(dot xs ys)

> transpose :: [[Integer]] -> [[Integer]]
> transpose mx = init (split (len mx) (transpose2 0 (len mx) mx))

> transpose2 :: Integer -> Integer -> [[Integer]] -> [Integer]
> transpose2 i n os = if i==(n*n) then [] else 
>   ((os !! (fromIntegral (i % n))) !! (fromIntegral (i `div` n))):(transpose2 (i+1) n os)

> split :: Integer -> [Integer] -> [[Integer]]
> split i is = if i>(len is) then [is] else do
>   let (as,bs) = splitAt (fromIntegral i) is
>   as:(split i bs)

> len :: [a] -> Integer
> len []     = 0
> len (a:as) = 1+(len as)

> (/%/) :: Integer -> Integer -> (Integer, Integer)
> a /%/ b = a `divMod` b

> (%) :: Integer -> Integer -> Integer
> a % b = a `mod` b

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b