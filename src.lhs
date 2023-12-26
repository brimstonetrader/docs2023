Merge Sort

> mergeSort :: [Integer] -> [Integer]
> mergeSort      [] = []
> mergeSort     [x] = [x]
> mergeSort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> mergeSort list    = collate (mergeSort a) (mergeSort b)
>      where (a, b) = splitAt ((length(list)) `div` 2) list

If the list has three or more elements, we split it, sort both, 
then collate. Haskell integer division is such that, if the list 
has an odd number of elements, the first will be larger.

> collate :: [Integer] -> [Integer] -> [Integer]
> collate        as []  = as
> collate (a:as) (b:bs) = if (a>b) then b:collate (a:as) bs 
>                                  else a:collate as (b:bs)

These two lists are sorted, so either a or b will be our s
mallest element. So, we will collate once for each list element.

Quick Sort

A similar idea, and an almost identical initial six lines.

> quickSort :: [Integer] -> [Integer]
> quickSort      [] = []
> quickSort     [x] = [x]
> quickSort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> quickSort list    = do
>   let (as, b:bs)  = splitAt ((length(list)) `div` 2) list
>   let [cs, ds]    = pivotAbout b as [[],[]]
>   let [es, fs]    = pivotAbout b bs [[],[]]
>   (quickSort (cs++es)) ++ (b:(quickSort (ds++fs)))

We pivot about the middle element, calling a new function that 
will cleave the list into two: one less, one greater, and put 
the two into that empty list we give it. We then call quickSort 
again, on both halves, ordering them correctly.

> pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>   then [x:a,b] else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>   then pivotAbout n xs [x:a,b] 
>   else pivotAbout n xs [a,x:b]

These two algorithms are both n(log(n))ish, as they both work by 
splitting the initial lists in two as many times as necessary. I 
used the below function to test them.

> list1ton :: Integer -> [Integer]
> list1ton n | n<1 = []
> list1ton n = n:(list1ton (n-1))

------------------------------------------------------------------

OPERATIONS ON SETS

All of this assumes your set has no repeated elements. If you aren’t 
sure, you can use this.

> removeRepeats :: [Integer] -> [Integer]
> removeRepeats [] = []
> removeRepeats (a:as) = if a `elem` as 
>                          then     removeRepeats as 
>                          else a : removeRepeats as

ghci> removeRepeats [1,1,1,2,2,3,4,5,3,5]
[1,2,4,3,5]

To find the intersection of two sets we extract all those elements 
from as that are also in bs.

> intersection :: [Integer] -> [Integer] -> [Integer]
> intersection []     bs = []
> intersection (a:as) bs = if a `elem` bs 
>                            then a:(intersection as bs) 
>                            else intersection as bs

ghci> intersection [1,2,3] [2,3,4]
[2,3]

To find the union, we concatenate our lists, then remove repeats, 
then sort.

> union        :: [Integer] -> [Integer] -> [Integer]
> union as bs = mergeSort (removeRepeats (as ++ bs))
 
ghci> union [1,2,3] [2,3,4]
[1,2,3,4]

To find the difference, we find the intersection of as and bs, then 
only return the elements of as that aren’t in the intersection.

> difference :: [Integer] -> [Integer] -> [Integer]
> difference as bs = sort (difference2 as (intersection as bs))

> difference2 :: [Integer] -> [Integer] -> [Integer]
> difference2 []     bs = []
> difference2 (a:as) bs = if a `elem` bs 
>                           then difference2 as bs 
>                           else a:(difference2 as bs)

ghci> difference [1,2,3] [2,3,4]
[1]

The disjunction is the difference of the union and intersection.

> disjunction :: [Integer] -> [Integer] -> [Integer]
> disjunction as bs = difference (union as bs) (intersection as bs)

ghci> disjunction [1,2,3] [2,3,4]
[1,4]

These functions will also be useful later.

> len :: [Integer] -> Integer
> len []     = 0
> len (a:as) = 1+len as

ghci> len [1,2,3,4,5]
5

> rng :: Integer -> Integer -> [Integer]
> rng _ 1    = [1]
> rng seed i = do 
>   let n = inst seed 
>   ((((n `div` 1001) `mod` i)+1) : (rng n (i-1)))

ghci> rng 5641421 9
[3,4,6,4,3,4,1,2,1]
ghci> rng 235234 9 
[5,1,1,1,3,1,2,1,1]
ghci> rng 234 9   
[2,1,1,5,5,1,3,1,1]
ghci> rng 2331414 9
[5,5,7,5,5,1,3,1,1]

> inst :: Integer -> Integer
> inst r = ((5397*r+7901) `mod` 65536) 

It is now time for the unary operation, permutation of a set. The 
seed lets us get our array of random numbers from the linear 
congruential generator.

> permute :: Integer -> Integer -> [Integer]
> permute seed n = do 
>   let as = list1ton n
>   let bs = rng seed n
>   permute2 as bs []

> permute2 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
> permute2 as [] cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

ghci> permute 65273853 9
[9,2,8,1,5,4,7,6,3]
ghci> permute 325321 4
[1,4,3,2]
ghci> permute 653 9  
[6,4,8,2,3,7,1,5,9]
ghci> permute 23198953 50 
[1,5,37,42,46,39,10,48,40,33,29,15,2,16,38,32,
 8,21,34,7,6,18,23,12,31,24,47,30,3,26,45,9,14,
 22,28,44,27,17,36,20,11,49,19,13,25,4,43,41,35,50]
ghci> len (permute 3248392 100)
100

-----------------------------------------------------------

e

One of the big operations John Conway uses for Surreal Numbers 
and Game Theory is calculating the mex of a set. This is short 
for “minimal excluded element”. mex(1,5,7,2,3) = 4. It is quite 
simple. Let us ask a reasonable question. What is the average 
mex of a set of $n$ random numbers, selected from an interval 
spanning from 1 to $n$, inclusive? The first two cases are 
simple to work out by hand:

mexavg(1) = 2, as the only possible set is [1].

mexavg(2) = 2.25, four equally likely sets, [[1,1],[1,2],[2,1],
[2,2]], have mexes of [2,3,3,1], which averages to 2.25.

I wrote a bit of code instead of analyzing the 27 3-cases by hand. 
With this data, as demonstrated below, I conjecture that this value 
approaches $e$ for sufficiently large values of$n$. Note that the 
probability that any given number is not included is ((n-1)^n / n^n), 
and that e = lim (n→∞) ((n+1/n)^n). 

ghci> mexOfN 527987395 5

2.357142857142857

ghci> mexOfN 527987395 10

2.6470588235294117

ghci> mexOfN 527987395 20

2.7132169576059852

ghci> mexOfN 527987395 40

2.748906933166771

ghci> mexOfN 527987395 80

2.752382440243712

ghci> mexOfN 527987395 160

2.734229131674544

> mexOfN :: Int -> Int -> Double
> mexOfN seed n = do 
>   let rnds = (listMod (rng seed (n^3)) n)
>   avg (mexEach n rnds)

> mexEach :: Int -> [Int] -> [Double]
> mexEach _ [] = []
> mexEach n rnds = do 
>   let (as,bs) = splitAt n rnds
>   (mex as 1):(mexEach n bs)

> mex :: [Int] -> Int -> Double
> mex as m = if m `elem` as then (mex as (m+1)) 
>                           else realToFrac m

> avg :: [Double] -> Double
> avg ds = (sum' ds) / (realToFrac (len ds))

> sum' :: [Double] -> Double
> sum' []     = 0.0
> sum' (d:ds) = d+(sum' ds)

----------------------------------------------

