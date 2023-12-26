-- chapter 6 of schaum's probability

-- BINOMIAL

-- Suppose there exists an event with probability p of success and 1-p=q of failure. The probability of k successes in n trials is 

-- (n choose k) * (p ^ k) * (q ^ (n-k))

--  ^- there are this many ways of arranging k successes among n trials
--                 ^- the last two terms represent the probability for
--                    any one arrangement.

-- mu      = np
-- sigma^2 = npq

> itod :: Integer -> Double
> itod x = realToFrac x

> dtoi :: Double -> Integer
> dtoi x = round x 

> choose :: Integer -> Integer -> Integer
> choose n k = (fall n k) `div` (fact (n-k))

> fact :: Integer -> Integer
> fact 0 = 1
> fact 1 = 1
> fact n = n*(fact (n-1))

> fall :: Integer -> Integer -> Integer
> fall n k = if n==k then 1 else n*(fall (n-1) k)

> binompdf :: Integer -> Integer -> Double -> Double
> binompdf n k p = (itod (choose n k)) * ( p ** (itod k)) * ((1.0-p) ** (itod (n-k)))

ghci> binompdf 3 0 0.5
0.125
ghci> binompdf 3 1 0.5
0.375
ghci> binompdf 3 2 0.5
0.375
ghci> binompdf 3 3 0.5
0.125

-- As n grows bigger, this approaches a normal distribution. That looks like

--         1/
-- _________________ e^(-0.5 * (x-mu)^2 / sigma^2)
-- sigma * (2pi)^0.5



-- APPROXIMATING PI

-- The classical approach is to repeat the first three odd numbers twice, 
-- cut the list in half, and turn it into a fraction.

-- 355/113 = 3.1415929203539825

-- The continued fraction for pi shows that this is probably the best 
-- way to do it this tersely. One I find neat, if worse, is

-- (2 ** 0.5) + (3 ** 0.5) = 3.1462643699419726

-- Here's a modern, iterative approach.

> r :: Integer -> Double
> r x = (realToFrac x) ** 0.5

> y0 :: Double
> y0 = (r 2) - 1

> a0 :: Double
> a0 = 6 - (4 * (r 2))

> f :: Double -> Double 
> f y = (1 - (y ** 4)) ** 0.25

> borwein :: Integer -> Double 
> borwein i = borwein2 0 i y0 a0

> borwein2 :: Integer -> Integer -> Double -> Double -> Double
> borwein2 i j y a = if i==j then 1/a else do 
>   let y' = (1 - (f y))/(1 + (f y))
>   let a' = (a*((1 + y') ** 4)) - (2 ** (itod (2*i+3)))*y'*(1+y'+(y'**2))
>   borwein2 (i+1) j y' a'

-- borwein 3 = 3.141592653589792

-- This method converges like lightning.

-- More:

--   3.8,29,44 in base 60
--   22/7 (classic)
--   31 ** (1 / 3)
--  (2143 / 22) ** 0.25


-- e is kinder. The Taylor series is wicked easy.

> e :: Integer -> Double
> e i = if i==0 then 1 else (1/(itod (fact i))) + (e (i-1))





















-- here's a fun way to do it

e2 :: Integer -> Double
> e2 x = mexOfN (seeds !! (fromIntegral (x % 8))) x



> seeds :: [Integer]
> seeds = [927342245,4142344322,943243493,484322496,53232443,1324324986,283423421,2224332412]


> mexOfN :: Integer -> Integer -> Double
> mexOfN seed n = do 
>   let rnds = rng seed (n^3) n
>   avg (mexEach n rnds)

> mexEach :: Integer -> [Integer] -> [Double]
> mexEach _ [] = []
> mexEach n rnds = do 
>   let (as,bs) = splitAt (fromIntegral n) rnds
>   (mex as 1):(mexEach n bs)

> mex :: [Integer] -> Integer -> Double
> mex as m = if m `elem` as then (mex as (m+1)) else realToFrac m

> avg :: [Double] -> Double
> avg ds = (sum' ds) / (realToFrac (len ds))

> sum' :: [Double] -> Double
> sum' []     = 0.0
> sum' (d:ds) = d+(sum' ds)

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)























> inst :: Integer -> Integer
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> rng :: Integer -> Integer -> Integer -> [Integer]
> rng ornd 1  cap = [((inst ornd) // 79) % cap]
> rng ornd it cap = do 
>   let nrnd = inst ornd 
>   ((nrnd // 79) % cap : rng nrnd (it-1) cap)





> (%) :: Integer -> Integer -> Integer
> a % b = a `mod` b

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b

> (/%/) :: Integer -> Integer -> (Integer, Integer)
> a /%/ b = a `divMod` b

> (~) :: Integer -> Integer -> Integer
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))

-- The central limit theorem means we can just do an Irwin-Hall cop-out.

> normal :: Double -> Double -> Integer -> Double 
> normal m s2 seed = do 
>   let rs12 = rng seed 12 10000
>   (((realToFrac (sum rs12)) / 10000) - 6)*(s2**0.5) + m









