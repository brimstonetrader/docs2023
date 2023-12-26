> secondLargest :: [Int] -> Int
> secondLargest (a:b:list) = sL2 (if a>b then (a,b) else (b,a)) list

> sL2 :: (Int,Int) -> [Int] -> Int
> sL2 (a,b) []     = b
> sL2 (a,b) (c:cs) = sL2 (if c>b then if c>a then (c,a) else (a,c) else (a,b)) cs 

> phi :: Double
> phi = (1 + (5 ** 0.5)) / 2

> ihp :: Double
> ihp = (1 - (5 ** 0.5)) / 2

> fib :: Int -> Int
> fib i = round (((phi ** (realToFrac i)) - (ihp ** (realToFrac i))) / (5 ** 0.5))

> rr :: Int -> Int -> Int -> Int
> rr n k i = (n*(fib (i-2))) + (k*(fib (i-1)))

-- kudos to James Grime 

ghci> rr 2 1 1
2
ghci> rr 2 1 2
1
ghci> rr 2 1 3
3
ghci> rr 2 1 4
4
ghci> rr 2 1 5
7
ghci> rr 2 1 6
11
ghci> rr 2 1 7
18