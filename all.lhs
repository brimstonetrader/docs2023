grp

In this section we will be exploring collections. We will begin with lists, then 
sets and groups. We will then pivot to graphs, and other related data structures
such as trees and matrices. At last, we will analyze partitions of grids.

-- (I've got a shit ton of handwritten ramblings on polyominoes that i'm in the process of turning into Haskell. This page 
--  is under construction.)


yk, the combnatorial argument, to this day, is the only truly convincing way I've encountered of showing that 0^0 = 1. 
The characteristic function of {0} (aka a(n) = 0^n aka A000007) looks pretty goddamn stupid, but it's hard to refute 
that there exists one mapping from an empty set onto another empty set. 

Look, what I mean to say, is really, that "Polyomino" is too long of a word for the concept of "a subset of a tiling", 
which is a concept I myself spend a great deal of time considering. I will be referring to the general class, square, 
hexagonal, even 3.4.3.4.4, as merely "Ominos". I hope this does not offend anyone.

This chart from wikipedia rocks

 n    name        free    fixed
-------------------------------
 1 	monomino 	1 	 	1
 2 	domino 	    1 	 	2
 3 	tromino 	2 	 	6
 4 	tetromino 	5 	 	19
 5 	pentomino 	12 	 	63
 6 	hexomino 	35 	 	216
 7 	heptomino 	108 	760
 8 	octomino 	369 	2725
 9 	nonomino 	1285 	9910
 10 decomino 	4655 	36446
 11 undecomino 	17073 	135268
 12 dodecomino 	63600 	505861

We can think of an omino as a square list of lists of booleans, like

O      [1,0,0]
OO ->  [1,1,0]
O      [1,0,0]

We can have a single list, instead, that contains n^2 booleans, or even a n^2-bit number, but that might be extraneous. 
Let's say that a "well-formed" polyomino representation in this system always has at least one True in the top row, and
left-most column. With this in mind, we can convert the above sequence to a number like so

[1,0,0] -> [1,   2,  4]
[1,1,0] -> [8,  16, 32] -> 89
[1,0,0] -> [64,128,256]

This is actually bad, though, because if we're faced with this same polyomino on a 5x5, or worse, nxn grid, its number 
will have changed, meaning we know jack squat. A better method, which assigns a unique number to every polyomino no 
matter the grid size, would be more like this.

[1,0,0] -> [  1,  2,16]
[1,1,0] -> [  8,  4,32] -> 269
[1,0,0] -> [256,128,64]

This means that every natural number bijects to a particular form in the grid, and the grid size does not change this 
number. For a Form to be an Omino, in this notation, it must be

	- CONTIGUOUS (there exists a path crossing only adjacent grid cells from any cell in the omino to 
	              any other cell in the omino)
	  
	- MINIMAL (there exists a cell in the omino along the top row and leftmost column. Thus, this is 
			   the smallest number that can represent it in this system. If we do away with this one,
			   we have an infinite family of representations for any omino.)



> convert :: Int -> [Bool]
> convert 0 = []
> convert n = do 
>   let (nd,nm) = n `divMod` 2
>   if nm == 1 then True:(convert nd) else False:(convert nd)


 rearrange :: [a] -> [a]
 rearrange ns = do 
   let l  = len ns
   let r  = sq l
   let k = r*r-l
   rearrange2 1 1 r (ns++(rep k 0)) [[]]

 rearrange2 :: Int -> Int -> Int -> [a] -> [[a]] [a]
 rearrange2 i j r os ns = if j>r then ns else if i>(2*j-1) 
   then do 
     let (as,bs) = splitAt ((len ns) - 1) ns
     rearrange2 1 (j+1) r os (as++reverse bs) 
   else if i<j then else rearrange2 (i+1) j 
     

 showOmino :: [Bool] -> IO ()
 showOmino bs = do 
   let n = sq bs
   

-- A FREE Omino is still the same regardless of how its reflected or rotated. A FIXED Omino can be changed by 
-- either of those operations, but not reliably. For example, 

--  O
-- OOO is the same fixed Omino regardless of how you reflect or rotate it. Each fixed Omino can be bijected to a 
--  O  sequence of TURTLE STEPS, which are as follows.

> data Step1 where
>   Forward :: Step1
>   Lef     :: Step1
>   Righ    :: Step1

-- The free tetrominoes, better known as the cast of Tetris.

-- O     O            O      O
-- O     O     OO     OO     OO
-- O  ,  OO  , OO   ,  O  ,  O 
-- O       


> tOne   :: [Step1]
> tTwo   :: [Step1]
> tThree :: [Step1]
> tFour  :: [Step1]
> tFive  :: [Step1]

> tOne   = [Forward,                    Forward,        Forward]
> tTwo   = [Forward,                    Forward,   Lef, Forward]
> tThree = [Forward,               Lef, Forward,   Lef, Forward]
> tFour  = [Forward,               Lef, Forward,  Righ, Forward]
> tFive  = [Forward, Forward, Lef, Lef, Forward,   Lef, Forward]



-- A pattern that crops up often as we go higher than n=4 is "Forward, Left, Left, Forward," which only adds one new unit 
-- to our omino. Let's write a new ADT, shortening all to single letters, and calling this new thing a "loop", as it brings 
-- one back where they started. "Loop", unfortunately, begins with the same letter as "Left", so we'll call it "O" for 
-- obvious reasons.

> data Step where
>   F :: Step
>   L :: Step
>   R :: Step
>   O :: Step
                                                                                
   1                      
   1       22                     5        
   1        2     333      44     55       666 
   1        2       3      44      5        6 
   1     ,  2  ,    3   ,   4  ,   5    ,   6

                                                   
   7                                                 
  77              9       AA       BB       C              
   7       888    999      AA     BB       CCC                
   7     , 8 8 ,    9   ,   A  ,   B    ,   C                                           
                         
> p1  = [F,   F,   F,   F]
> p2  = [F,   F,   F,L, F]
> p3  = [F,   F,L, F,   F]
> p4  = [F,   F,L, F,L, F]
> p5  = [F,   F,L, F,R, F]
> p6  = [F,   F,L, O,   F]
> p7  = [F,   F,L, O,L, F]
> p8  = [F,L, F,   F,L, F]
> p9  = [F,L, F,   F,R, F]
> p10 = [F,L, F,R, F,L, F]
> p11 = [F,L, O,L, F,R, F]
> p12 = [F,L, O,L, O,L, F]

We can arbitrarily sort them by this ranking: 

[F  ->  F,L  ->  F,R  ->  O  ->  O,L  ->  O,R]

Reversing a list    of turtle steps creates the same polyomino, but rotated 180 degrees. 

Prefacing a list    of turtle steps with x Ls rotates an Omino by x*90 degrees

Replacing instances of L with R and vice versa reflects a polyomino about the y-axis.

ominoes in square-space are beholden to the symmetry group D_8, which has three generators 
that we'll concern ourselves with, r, rrs, and s. In group theory, in a group of form D_X, 
"r" means to rotate by 360/(X/2) degrees, and "s" means to reflect about the y-axis. "e"
means just regular style. Groups of form "D_X" catalog all possible symmetries of regular 
polygons. Below, a chart.

_|   e, r, rr, rrr, s, rs, rrs, rrrs  
1    1  0   1    0  1   0    1     0
2    1  0   0    0  0   0    0     0
3    1  0   0    0  0   1    0     0
4    1  0   0    0  0   0    0     0
5    1  0   0    0  0   0    0     0
6    1  0   0    0  1   0    0     0
7    1  0   0    0  0   0    0     0
8    1  0   0    0  1   0    0     0
9    1  0   1    0  0   0    0     0
A    1  0   0    0  0   1    0     0
B    1  0   0    0  0   0    0     0
C    1  1   1    1  1   1    1     1


one can plainly glean that the important bits are rr, s, and rs. They "generate" our other symmetries.
if we verify that a polyomino has or does not have some combination of the three, we can fill in the 
rest of the table. It's sort of like vertices on a cube, like 


     rs   -  r
   /  |      |
- e  rrrs -  rrr
  | /       /
  rr  - rrs

who's hiding? 


being symmetric about s means y=0 is your line of symmetry. rr means y=x is, and rs means y=-x.















[]

> printOmino :: Char -> [[Bool]] -> String
> printOmino a ((b:[]):[])  = if b then [a] ++ "\n"                  else "\n"  
> printOmino a ([]:bss)     = "\n" ++ printOmino a bss   
> printOmino a ((b:bs):[])  = if b then a:printOmino a (bs:[])       else ' ':printOmino a (bs:[])  
> printOmino a ((b:bs):bss) = if b then a:(printOmino a (bs:bss))    else ' ':printOmino a (bs:bss)  

-- ghci> putStr (printOmino 'X' [[False,True,False],[True,True,True],[True,False,False]])
--  X
-- XXX
-- X

 split :: Int -> Int -> [[Bool]] -> [[Bool]]
 split x y grid = do 
   



> square :: Int -> [a] -> [[a]]
> square s [] = [] 
> square s as = do 
>   let (bs,cs) = splitAt s as
>   bs:(square s cs)
 
> monomino :: Int -> [[Bool]]
> monomino n = init (emptyGrid2 0 n )

> emptyGrid2 :: Int -> Int -> [[Bool]]
> emptyGrid2 i n = if i==n-1 
>   then (True:(rep (n-1) False)):(emptyGrid2 (i+1) n) 
>   else if i<n 
>     then (rep n False):(emptyGrid2 (i+1) n) 
>     else [[]]




 randOmino :: Int -> Int -> IO ()
 randOmino seed n = do 
   let rnds = (boolNoise seed 1000)
   putStr (printOmino (square n rnds))




> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)<y then sqrt2 (x+1) y else x

> maxlength :: [[a]] -> Int
> maxlength [[]] = 0
> maxlength [as] = len as
> maxlength (as:bs:css) = do 
>   let la = len as
>   let lb = len bs
>   if (la>lb) then maxlength (as:css) else maxlength (bs:css) 

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)


-- ghci> printGrid 10

	_ _ _ _ _ _ _ _ _ _
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 79) `mod` 2):(listMod as b)

 boolNoise :: Int -> [Bool]
 boolNoise n c = do 
   let xs = (listMod (lcg n c) 2)
   i2b xs

> i2b :: [Int] -> [Bool]
> i2b []     = []
> i2b (0:is) = False:(i2b is)
> i2b (1:is) =  True:(i2b is)
> i2b (c:is) =       (i2b ((c `mod` 2):is))

> type Matrix = [[Int]] 

> zeroMatrix :: Int -> Matrix
> zeroMatrix n = (rep n (rep n 0))

> identityMatrix :: Int -> Matrix
> identityMatrix n = identityMatrix2 0 n 

> identityMatrix2 :: Int -> Int -> Matrix
> identityMatrix2 i n = if i<n then 
>   ((amanybs i 0) ++ (1:(amanybs (n-i-1) 0))):(identityMatrix2 (i+1) n) 
>   else []

> transpose :: Matrix -> Matrix
> transpose mx = init (split (len mx) (transpose2 0 (len mx) mx))

> split :: Int -> [Int] -> [[Int]]
> split i is = if i>(len is) then [is] else do
>   let (as,bs) = splitAt i is
>   as:(split i bs)

> transpose2 :: Int -> Int -> [[Int]] -> [Int]
> transpose2 i n os = if i==(n*n) then [] else 
>   ((os !! (i `mod` n)) !! (i `div` n)):(transpose2 (i+1) n os)

 (#+) :: Matrix -> Matrix -> Matrix 
 mx #+ my = 




> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b

> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b



> isPrime :: Int -> Bool 
> isPrime p = if p `mod` 2 == 0 then False 
>               else isPrime2 p 3 (sq p) 

> isPrime2 :: Int -> Int -> Int -> Bool 
> isPrime2 p i n = if i>n  then True 
>   else if p `mod` i == 0 then False 
>                          else isPrime2 p (i+2) n

> nextPrime :: Int -> Int  
> nextPrime n = if isPrime n then n else nextPrime (n+1)


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

> randPath :: Int -> Int -> IO ()
> randPath seed n = do 
>   let rnds = permute seed [1..16]
>   putStr (convert 0 n rnds)

> data Move where
>   Diag :: Limit  -> Move
>   Orth :: Limit  -> Move
>   Hipp :: Coord  -> Limit -> Move
>   Mult :: [Move] -> Move

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   List  :: [Int]      -> Limit
>   None  ::               Limit

> type Coord = (Int, Int)


> convert :: Int -> Int -> [Int] -> String
> convert i n [] = "\n"
> convert i n (r:rnds) = if i%n==n-1 then ((intToChar r):"\n") ++ (convert (i+1) n rnds) else (intToChar r):(convert (i+1) n rnds)


> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)


> (~) :: Int -> Int -> Int
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))


> intToChar :: Int -> Char
> intToChar 1  = '0'
> intToChar 2  = '1'
> intToChar 3  = '2'
> intToChar 4  = '3'
> intToChar 5  = '4'
> intToChar 6  = '5'
> intToChar 7  = '6'
> intToChar 8  = '7'
> intToChar 9  = '8'
> intToChar 10 = '9'
> intToChar 11 = 'A'
> intToChar 12 = 'B'
> intToChar 13 = 'C'
> intToChar 14 = 'D'
> intToChar 15 = 'E'
> intToChar 16 = 'F'
> intToChar x  = intToChar (x%16)




All of this assumes your set has no repeated elements. If you aren’t sure, you can use this.

> removeRepeats :: [Int] -> [Int]
> removeRepeats [] = []
> removeRepeats (a:as) = if a `elem` as then removeRepeats as else a:(removeRepeats as)

ghci> removeRepeats [1,1,1,2,2,3,4,5,3,5]
[1,2,4,3,5]




To find the intersection of two sets we extract all those elements from as that are also in bs.

> intersection :: [Int] -> [Int] -> [Int]
> intersection []     bs = []
> intersection (a:as) bs = if a `elem` bs then a:(intersection as bs) else intersection as bs

ghci> intersection [1,2,3] [2,3,4]
[2,3]

To find the union, we concatenate our lists, then remove repeats, then sort.

> union        :: [Int] -> [Int] -> [Int]
> union as bs = removeRepeats (as ++ bs)
 
ghci> union [1,2,3] [2,3,4]
[1,2,3,4]

To find the difference, we find the intersection of as and bs, then only return the elements of as that aren’t in the intersection.

> difference :: [Int] -> [Int] -> [Int]
> difference as bs = sort (difference2 as (intersection as bs))

> difference2 :: [Int] -> [Int] -> [Int]
> difference2 []     bs = []
> difference2 (a:as) bs = if a `elem` bs then difference2 as bs else a:(difference2 as bs)

ghci> difference [1,2,3] [2,3,4]
[1]

The disjunction is the difference of the union and intersection.

> disjunction :: [Int] -> [Int] -> [Int]
> disjunction as bs = difference (union as bs) (intersection as bs)

ghci> disjunction [1,2,3] [2,3,4]
[1,4]

These functions will also be useful later.



ghci> len [1,2,3,4,5]
5

> list1ton :: Int -> [Int]
> list1ton n | n<1 = []
> list1ton n = n:(list1ton (n-1))


> (@) :: Int -> [Int] -> Bool
> a @ []     = False
> a @ (b:bs) = if a==b then True else a @ bs

> (*<) :: [Int] -> [Int] -> Bool 
> as *< bs = contains2 (sort bs) (sort as)

> (>*) :: [Int] -> [Int] -> Bool 
> as >* bs = contains2 (sort as) (sort bs)


> contains2 :: [Int] -> [Int] -> Bool
> contains2 _ []         = True
> contains2 (a:as) (b:bs) = if a>b then False 
>   else if a<b then contains2 (a:as) bs 
>               else contains2    as  bs


ghci> list1ton 10
[10,9,8,7,6,5,4,3,2,1]

ghci> lcg 5641421 9
[3,4,6,4,3,4,1,2,1]
ghci> lcg 235234 9 
[5,1,1,1,3,1,2,1,1]
ghci> lcg 234 9   
[2,1,1,5,5,1,3,1,1]
ghci> lcg 2331414 9
[5,5,7,5,5,1,3,1,1]


It is now time for the unary operation, permutation of a set. The seed lets us get our array of random numbers from the linear congruential generator.

 permute :: Int -> Int -> [Int]
 permute seed n = do 
   let as = list1ton n
   let bs = lcg seed n
   permute2 as bs []

 permute2 :: [Int] -> [Int] -> [Int] -> [Int]
 permute2 as [] cs = cs 
 permute2 as (b:bs) cs = do
   let n          = len as
   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
   permute2 (ds++es) bs (e:cs) 

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






   







 randOmino :: Int -> Int -> IO ()
 randOmino seed n = do 
   let rnds = (boolNoise seed 1000)
   putStr (printOmino (square n rnds))





-- ghci> printGrid 10

	_ _ _ _ _ _ _ _ _ _
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   
A grid is a singly linked list which looks like this

[0, 1, 2, 3, 4,
 5, 6, 7, 8, 9,
10,11,12,13,14,
15,16,17,18,19,
20,21,22,23,24]

You can figure out who's in your (row,column) with Euclidean Division.

A Move from one unit to another consists of a positive or negative number of shifts in x and y. We can represent this as a Vector, or ordered pair.


 data Move where
   Sing :: Vector -> Sym -> Limit -> Move
   List :: [Move] -> Move

 data Limit where
   Num   :: Int        -> Limit
   Range :: (Int, Int) -> Limit
   Any   :: [Int]      -> Limit
   No    ::               Limit

 data Sym where
   None :: Sym 
   Line :: Sym 
   Quad :: Sym 
   Octo :: Sym 

> type Vector = (Int, Int)

we can think of each number representing a movement representable by a tuple of numbers. 
Diagonal moves look like (x,x), Orthogonal moves look like (0,x) or (x,0). Hippogonal 
moves look like (x,y). Here are the chess pieces with this abstract syntax.


> pMove = Sing (0,1) None (Num 1)
> rMove = Sing (0,1) Quad  No
> nMove = Sing (1,2) Octo (Num 1)
> bMove = Sing (1,1) Quad  No
> qMove = Sing (0,1) Octo  No
> kMove = Sing (0,1) Octo (Num 1)


with that in mind, we can permute a list of integers to get something akin to a 
"knight's tour". This is a sequence of knight moves on a chess-board that traverse 
the entire board without stopping on the same square twice. Since this is truly random,
this is a teleporting knight's tour.

> rpt :: Int -> Int -> [Int]
> rpt seed n = do 
>   let rnds = permute seed [2..(n*n-1)]
>   ((1:rnds)++[n*n])

ghci> rpt 13123213 5
	
	[1,13,20,4,9,2,11,21,14,12,16,3,18,7,19,22,10,8,6,5,15,17,24,23,25]

it's notable that there are two valid ways of reading these: as a sequence
of addresses, or as a sequence of times. That didn't make much sense at all.
Consider the n=3 case.

ghci> permute 890410 [1..9]
	[7,3,8,1,9,5,2,6,4]
	
We take the initial seven to mean that 1 is visited on the seventh moment of the tour.
The 3 in place 2 means that three is visited second, and so on. Thus, we want

	f([7,3,8,1,9,5,2,6,4]) = 
	  [4,7,2,9,6,8,1,3,5]

> switch :: [Int] -> [Int]
> switch ns = switch2 1 (len ns) ns

> switch2 :: Int -> Int -> [Int] -> [Int]
> switch2 i m ns = if i>m then [] else (1+whereIs i ns):(switch2 (i+1) m ns)

ghci> switch [1,13,20, 4, 9, 2,11,21,14,12,16, 3,18, 7,19,22,10, 8, 6, 5,15,17,24,23,25]
			 [1, 6,12, 4,20,19,14,18, 5,17, 7,10, 2, 9,21,11,22,13,15, 3, 8,16,24,23,25]

This is an interesting operation to do on a permutation because it is entirely reversible. Look,

ghci> switch [1, 6,12, 4,20,19,14,18, 5,17, 7,10, 2, 9,21,11,22,13,15, 3, 8,16,24,23,25]
			 [1,13,20, 4, 9, 2,11,21,14,12,16, 3,18, 7,19,22,10, 8, 6, 5,15,17,24,23,25]
			 
This inclines me to believe that "switching" a permutation allows us to find something like its inverse.
We can now measure a permutation in terms of its cycles, like


	[3,1,5,2,6,4]  2 <-- 3  1 <-- 5   -This is written (145)(236)
	[2,4,1,6,3,5]  \> 6 -^  \> 4 -^   

From these two permutations, we can write two more, like

	[3,1,5,2,6,4] at index [2,4,1,6,3,5] -> [5,3,6,1,4,2]
	[2,4,1,6,3,5] at index [3,1,5,2,6,4] -> [4,6,2,5,1,3]
	
This operation is fun. For any permutation, [x,y,z...] at index [x,y,z...] -> [1,2,3...].
So any permutation, under this operation, is its own inverse. Additionally, for any permutation,
[x,y,z...] at index [1,2,3...] -> [x,y,z...], and [1,2,3...] at index [x,y,z...] -> [x,y,z...]
	
> (@) :: [Int] -> [Int] -> [Int] 
> as @ bs = do 
>   let (la,lb) = (len as, len bs) 
>   atIndex (lb-la) la as bs
	
> atIndex :: Int -> Int -> [Int] -> [Int] -> [Int]
> atIndex i n as bs = if i==n then [] else (as !! (whereIs (i+1) bs)):(atIndex (i+1) n as bs) 
	
Our loop structure is the same - the values are all the same. There are two loops containing
three values each, [2,3,6] and [1,4,5]. In general, a @ (switch a) = reverse (switch a). 

ghci> [3,1,5,2,6,4] @ [2,4,1,6,3,5]
									[5,3,6,1,4,2]
ghci> [2,4,1,6,3,5] @ [3,1,5,2,6,4]
									[4,6,2,5,1,3]
ghci> [3,1,5,2,6,4] @ [1,2,3,4,5,6]
									[3,1,5,2,6,4]                 
ghci> [5,3,6,1,4,2] @ [6,5,4,3,2,1]
									[2,4,1,6,3,5]	

This operation is not abelian. a @ b does not equal b @ a. Here it is on two permutations that are 
not related, with different cycles. 
		
ghci> [5,2,4,3,6,1] @ [4,1,2,6,3,5]
									[2,4,6,5,1,3]
ghci> [4,1,2,6,3,5] @ [5,2,4,3,6,1]
									[5,1,6,2,4,3]
		

All of these permutations are isomorphic under S_6, representable as (145)(236) = (14)(45)(51)(23)(36)(62). 


	
> randPathTelep :: Int -> Int -> IO ()
> randPathTelep seed n = do 
>   let rnds = permute seed [2..(n*n-1)]
>   printTour n ((1:rnds)++[n*n])

> printTour :: Int -> [Int] -> IO ()
> printTour _ [] = putStr "\n"
> printTour n as = if n>len as then putStrLn (show as) else do 
>   let (bs,cs) = splitAt n as
>   putStrLn (show bs)
>   printTour n cs

We can convert this into a list of vectors.

> catalog :: [Int] -> [Vector]
> catalog ns = catalog2 (sq (len ns)) ns  

> catalog2 :: Int -> [Int] -> [Vector]
> catalog2 _ [a]      = []
> catalog2 n (a:b:cs) = do 
>   let (dr,dc) = (b%n - a%n,a//n - b//n)
>   let r = if a%n  - dc < -1 then dr+n else if a%n  - dc >= n then dr-n else dr
>   let c = if a//n - dr < -1 then dc+n else if a//n - dr >= n then dc-n else dc 
>   (r,c):(catalog2 n (b:cs)) 

ghci> randPathTelep 234423324 3
  [1,7,6]
  [8,4,5]
  [2,3,9]

ghci> catalog (switch [1,7,6,8,4,5,2,3,9])
  [(0,-2),(1,0),(0,1),(-2,-1),(0,1),(2,1),(-1,-1),(-1,-2)]

ghci> printTour 5 [1,18,11,20,7,12,3,23,21,17,22,9,14,2,13,15,24,6,10,5,4,19,16,8,25]

  [ 1,18,11,20,7]
  [12, 3,23,21,17]
  [22, 9,14, 2,13]
  [15,24, 6,10,5]
  [ 4,19,16, 8,25]
  
ghci> catalog (switch   [ 1,18,11,20,7]
						[12, 3,23,21,17]
						[22, 9,14, 2,13]
						[15,24, 6,10,5]
						[ 4,19,16, 8,25])

  [(3,-2),(-2,1),(-1,-3),(-1,0),(3,1),
   (-3,2),(4,-3),(-2,2),(2,-1),(-1,3),
   (-2,-1),(-1,-2),(3,1),(-2,-1),(2,-1),
   (-3,2),(2,2),(0,-4),(2,4),(0,-1),
   (-3,-1),(2,1),(-1,-2),(-2,-2)]
 
There's a fascinating issue here - these vectors work perfectly, provided you embed 
your grid to a torus.



-- HELPERS

> intToChar :: Int -> Char
> intToChar 1  = '0'
> intToChar 2  = '1'
> intToChar 3  = '2'
> intToChar 4  = '3'
> intToChar 5  = '4'
> intToChar 6  = '5'
> intToChar 7  = '6'
> intToChar 8  = '7'
> intToChar 9  = '8'
> intToChar 10 = '9'
> intToChar 11 = 'A'
> intToChar 12 = 'B'
> intToChar 13 = 'C'
> intToChar 14 = 'D'
> intToChar 15 = 'E'
> intToChar 16 = 'F'
> intToChar 17 = 'G'
> intToChar 18 = 'H'
> intToChar 19 = 'I'
> intToChar 20 = 'J'
> intToChar 21 = 'K'
> intToChar 22 = 'L'
> intToChar 23 = 'M'
> intToChar 24 = 'N'
> intToChar 25 = 'O'
> intToChar 26 = 'P'
> intToChar 27 = 'Q'
> intToChar 28 = 'R'
> intToChar 29 = 'S'
> intToChar 30 = 'T'
> intToChar 31 = 'U'
> intToChar 32 = 'V'
> intToChar 33 = 'W'
> intToChar 34 = 'X'
> intToChar 35 = 'Y'
> intToChar 36 = 'Z'
> intToChar x  = intToChar (x%16)

> convert :: Int -> Int -> [Int] -> String
> convert i n [] = ""
> convert i n (r:rnds) = if (i-3)%n==0 
>   then ((intToChar r):"\n") ++ (convert (i+1) n rnds) 
>   else (intToChar r):(convert (i+1) n rnds)

-- THE SECTION I COPY/PASTE INTO EVERY NEW HASKELL DOCUMENT
-- DO NOT READ UNLESS YOU ARE A COMPUTER



> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)


> (~) :: Int -> Int -> Int
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))

> sort :: [Int] -> [Int]
> sort      [] = []
> sort     [x] = [x]
> sort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> sort list    = do
>   let (as, b:bs)  = splitAt ((len list) `div` 2) list
>   let [cs, ds]    = pivotAbout b as [[],[]]
>   let [es, fs]    = pivotAbout b bs [[],[]]
>   (sort (cs++es)) ++ (b:(sort (ds++fs)))

> pivotAbout :: Int -> [Int] -> [[Int]] -> [[Int]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]



> removeRepeats :: [Int] -> [Int]
> removeRepeats [] = []
> removeRepeats (a:as) = if a `elem` as then removeRepeats as else a:(removeRepeats as)

> intersection :: [Int] -> [Int] -> [Int]
> intersection []     bs = []
> intersection (a:as) bs = if a `elem` bs then a:(intersection as bs) else intersection as bs

> union        :: [Int] -> [Int] -> [Int]
> union as bs = removeRepeats (as ++ bs)

> difference :: [Int] -> [Int] -> [Int]
> difference as bs = sort (difference2 as (intersection as bs))

> difference2 :: [Int] -> [Int] -> [Int]
> difference2 []     bs = []
> difference2 (a:as) bs = if a `elem` bs then difference2 as bs else a:(difference2 as bs)

> disjunction :: [Int] -> [Int] -> [Int]
> disjunction as bs = difference (union as bs) (intersection as bs)

> refl :: Int -> Int -> [Int] -> [Int]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i `mod` n)) + (i `div` n)))):(refl (i+1) n os)

> isPrime :: Int -> Bool 
> isPrime p = if p `mod` 2 == 0 then False 
>               else isPrime2 p 3 (sq p) 

> isPrime2 :: Int -> Int -> Int -> Bool 
> isPrime2 p i n = if i>n  then True 
>   else if p `mod` i == 0 then False 
>                          else isPrime2 p (i+2) n

> nextPrime :: Int -> Int  
> nextPrime n = if isPrime n then n else nextPrime (n+1)

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)<y then sqrt2 (x+1) y else x

> ack :: Int -> Int -> Int
> ack 0 n = n+1
> ack m 0 = ack (m-1) 1
> ack m n = ack (m-1) (ack m (n-1))

> factorial :: Int -> Int -> Int
> factorial 0 k = k
> factorial n k = factorial (n-1) (k*n)

> choose :: Int -> Int -> Int
> choose n k = (factorial n 1) `div` ((factorial (n-k) 1)*(factorial k 1))

> fall :: Int -> Int -> Int
> fall n k = (factorial n 1) `div` (factorial (n-k) 1)

> stir :: Int -> Int -> Int
> stir n k =    if n==k then 1 
>   else if k==0 || k>n then 0 
>   else (k*(stir (n-1) k)) + (stir (n-1) (k-1)) 

> whereIs :: Int -> [Int] -> Int
> whereIs a bs = wI2 a bs 0

> wI2 :: Int -> [Int] -> Int -> Int
> wI2 a []     c = -1
> wI2 a (b:bs) c = if a==b then c else wI2 a bs (c+1)



> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 79) `mod` 2):(listMod as b)

 boolNoise :: Int -> [Bool]
 boolNoise n c = do 
   let xs = (listMod (lcg n c) 2)
   i2b xs

> i2b :: [Int] -> [Bool]
> i2b []     = []
> i2b (0:is) = False:(i2b is)
> i2b (1:is) =  True:(i2b is)
> i2b (c:is) =       (i2b ((c `mod` 2):is))

A quick one, for today.

One of the big operations John Conway uses for Surreal Numbers and Game Theory is calculating the mex of a set. This is short for “minimal excluded element”. mex(1,5,7,2,3) = 4. It is quite simple. Let us ask a reasonable question. What is the average mex of a set of $n$ random numbers, selected from an interval spanning from 1 to $n$, inclusive? The first two cases are simple to work out by hand:

mexavg(1) = 2, as the only possible set is [1].

mexavg(2) = 2.25, four equally likely sets, [[1,1],[1,2],[2,1],[2,2]], have mexes of [2,3,3,1], which averages to 2.25.

I wrote a bit of code instead of analyzing the 27 3-cases by hand. With this data, as demonstrated below, I conjecture that this value approaches $e$ for sufficiently large values of $n$.

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


	> rng :: Int -> Int -> [Int]
	> rng ornd 1  = [(inst ornd)]
	> rng ornd it = do 
	>   let nrnd = inst ornd 
	>   (nrnd : rng nrnd (it-1))

	> listMod :: [Int] -> Int -> [Int]
	> listMod [] _ = []
	> listMod (a:as) b = (((a `div` 79) `mod` b)+1):(listMod as b)

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
	> mex as m = if m `elem` as then (mex as (m+1)) else realToFrac m

	> avg :: [Double] -> Double
	> avg ds = (sum' ds) / (realToFrac (len ds))

	> sum' :: [Double] -> Double
	> sum' []     = 0.0
	> sum' (d:ds) = d+(sum' ds)



  > tangent :: Double -> Double
        > tangent x = (sine x) / (cosine x)
        >
        > cosine :: Double -> Double
        > cosine x = sine (x+(3.14159265359 / 2))
        >
        > bhaskara :: Double -> Double
        > bhaskara x = (16*x*(3.14159265359 - x))/((5*(3.14159265359 ** 2))-(4*x*(3.14159265359 - x)))
        >
        > sine :: Double -> Double 
        > sine 0 = 0
        > sine x = case ((modulus x 6.28318530718)>3.1415926535) of 
        >   False -> (truncate' (bhaskara (modulus x 6.28318530718)) 4)
        >   True  -> (truncate' ((-1) * bhaskara ((modulus x 6.28318530718)-3.1415926535)) 4)
  > part :: Integer -> Integer -> Integer
        > part _ 1 = 1
        > part n k | n < k = 0 
        > part n k = (part (n-1) (k-1)) + (part (n-k) (k))
        >
        > partition :: Integer -> Integer
        > partition 0 = 1
        > partition 1 = 1
        > partition 2 = 2
        > partition 3 = 3
        > partition 4 = 5
        > partition 5 = 7
        > partition 6 = 11
        > partition n = (partition (n-1)) + (partition (n-2)) - (partition (n-5)) 
        >             - (partition (n-7)) + (partitionHelper0 (n-12) 0) 

        PROOF:

        0    1     11   111   1111   11111     111111
                    2    21    211    2111      21111
                          3     22     221       2211
                                31     311       3111
                                 4      41        411
                                        23        321
                                         5        222
                                                   51
                                                   42
                                                   33
                                                    6 

ARCHIVE
O|O|O|O|O|O|O|O|O|O|O|O| |O|O|O|O|O|O|O|O|O|O|O|O O|O|O|O|O|O|O|O|O|O|O|O|
View on GitHub

	----------------------------------------------------------------
	--
	-- Nonogram


----------------------------------------------------------------
	-- Random Number Generator
	--
	-- Standard
	-- 
	--  Chapter 7.1, Eq. 7.1.6
	--  parameters from Knuth and H. W. Lewis


	> rng :: Int -> Int -> [Int]
	> rng ornd 1  = [(inst ornd)]
	> rng ornd it = do 
	>   let nrnd = inst ornd 
	>   (nrnd : rng nrnd (it-1))

	-- Has disappointingly small modular periods, but
	-- an Int division of 99 seems to clean it up.

	> listMod :: [Int] -> Int -> [Int]
	> listMod [] _ = []
	> listMod (a:as) b = ((a `div` 99) `mod` 2):(listMod as b)



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


	-- Reflects a square list 45 degrees, like

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

	> ed :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
	> ed i n r rs []     = r:rs
	> ed i n r rs (q:qs) = if i `mod` n == 0 
	>                      then ed (i+1) n 0     (q+r:rs) qs 
	>                      else ed (i+1) n (q+r)    rs  qs

	> nonogram :: Int -> Int -> (([Int], [Int]), [Int])
	> nonogram seed n = do
	>   let rnds = (listMod (rng seed (n*n)) 2)
	>   let c:cs = ed 1 n 0 [] rnds
	>   let r:rs = ed 1 n 0 [] (refl 0 n rnds)
	>   ((reverse rs,reverse cs),rnds)

	-- ghci> nonogram 687248978 6
	       (([3,4,4,4,3,3],
		 [4,6,1,2,3,5]),
	  
		 [0,1,1,1,1,0,
		  1,1,1,1,1,1,
		  0,1,0,0,0,0,
		  1,0,0,1,0,0,
		  0,0,1,1,0,1,
		  1,1,1,0,1,1])

	-- Prints a blank grid of size n.

	> printGrid :: Int -> IO ()
	> printGrid n = do 
	>   putStr ((rep n " _") ++ "\n")
	>   printGrid2 n n

	> printGrid2 :: Int -> Int -> IO ()
	> printGrid2 n 1 = putStr (('|':(rep n "_|")) ++ "\n")
	> printGrid2 n i = do 
	>   putStr (('|':(rep n "_|")) ++ "\n")
	>   printGrid2 n (i-1)


	-- ghci> printGrid 10

            _ _ _ _ _ _ _ _ _ _
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|
	   |_|_|_|_|_|_|_|_|_|_|

	-- Prints hints on blank grid.

	> unsPuzzle :: Int -> Int -> IO ()
	> unsPuzzle seed n = do
	>   let ((as,bs),cs) = nonogram seed n
	>   let s1 = (toStr as) ++ "\n"
	>   putStr (' ':s1)
	>   printGridU bs n n

	> printGridU :: [Int] -> Int -> Int -> IO ()
	> printGridU (m:ms) n 1 = putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
	> printGridU (m:ms) n i = do 
	>   putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
	>   printGridU ms n (i-1)

	-- Prints hints onto solved grid.

	> slvPuzzle :: Int -> Int -> IO ()
	> slvPuzzle seed n = do
	>   let ((as,bs),cs) = nonogram seed n
	>   let s1 = ' ':(toStr as)
	>   putStr (' ':s1)
	>   printGridS bs cs n (n*n)

	> printGridS :: [Int] -> [Int] -> Int -> Int -> IO ()
	> printGridS [] [m]    _ _ = putStr (if m == 1 then "X|" else "_|")
	> printGridS [] (m:ms) n i = do 
	>   putStr (if m == 1 then "X|" else "_|")
	>   printGridS [] ms n (i-1)
	> printGridS (l:ls) (m:ms) n i = do 
	>   let s = if m == 1 then "X|" else "_|"
	>   let t = if i `mod` n == 0 
	>           then ("\n" ++ (if l < 10 
	>                          then (show l) ++ " " 
	>                          else show l) ++ "|") 
	>           else ""
	>   putStr t
	>   putStr s
	>   printGridS (if t=="" then (l:ls) else ls) ms n (i-1) 

	-- ghci> unsPuzzle 87632786 10

	  8 4 6 3 5 5 5 6 6 3
	7|_|_|_|_|_|_|_|_|_|_|
	6|_|_|_|_|_|_|_|_|_|_|
	3|_|_|_|_|_|_|_|_|_|_|
	4|_|_|_|_|_|_|_|_|_|_|
	5|_|_|_|_|_|_|_|_|_|_|
	3|_|_|_|_|_|_|_|_|_|_|
	4|_|_|_|_|_|_|_|_|_|_|
	7|_|_|_|_|_|_|_|_|_|_|
	6|_|_|_|_|_|_|_|_|_|_|
	6|_|_|_|_|_|_|_|_|_|_|

	-- ghci> slvPuzzle 87632786 10

	   8 4 6 3 5 5 5 6 6 3
	7 |X|X|X|_|X|_|_|X|X|X|
	6 |X|_|_|X|X|X|X|_|X|_|
	3 |_|_|_|X|X|_|X|_|_|_|
	4 |X|_|X|_|_|_|X|X|_|_|
	5 |X|X|_|_|_|_|_|X|X|X|
	3 |_|X|_|_|_|X|_|_|X|_|
	4 |X|_|X|_|_|X|X|_|_|_|
	7 |X|X|X|_|X|X|_|X|X|_|
	6 |X|_|X|_|X|X|_|X|X|_|
	6 |X|_|X|X|_|_|X|X|_|X|














	-------------------------------------------------------------------------------
	-- To make these nonograms real, we need not total count, but counts of groups.
	-- So [1,1,0,0,1,0,1,1,1,0] -> [2,1,3] nstad 

	> runsOfOnes :: [Int] -> [Int]
	> runsOfOnes as = reverse (runsOfOnes2 0 as [])

	> runsOfOnes2 :: Int -> [Int] -> [Int] -> [Int]
	> runsOfOnes2 i [] bs = if i>0 then i:bs else bs
	> runsOfOnes2 i (1:as) bs = runsOfOnes2 (i+1) as bs
	> runsOfOnes2 i (a:as) bs = if i>0 then runsOfOnes2 0 as (i:bs) 
	>                                  else runsOfOnes2 0 as bs 

	> applyROO :: Int -> [Int] -> [[Int]]
	> applyROO n [] = [[]]
	> applyROO n ns = (runsOfOnes (extract n ns)):(applyROO n (retract n ns))

	> nonoGram :: Int -> Int -> (([[Int]], [[Int]]), [Int])
	> nonoGram seed n = do
	>   let rnds = (listMod (rng seed (n*n)) 2)
	>   let rs = init (applyROO n rnds)
	>   let cs = init (applyROO n (refl 0 n rnds))
	>   ((cs,rs),rnds)

	-- ghci> nonoGram 87328678 4

		(([[  3],[  1],[1],[2]],
		  [[1,2],[2,1],[1],[ ]]
		   
		       ),[1,0,1,1,
			  1,1,0,1,
			  1,0,0,0,
			  0,0,0,0])



	-- Adds zeroes to front of sublists as needed, making each the same length

	> pad :: [[Int]] -> [[Int]]
	> pad xss = do
	>   let m = maxlength xss 
	>   reverse (pad2 m xss [])

	> pad2 :: Int -> [[Int]] -> [[Int]] -> [[Int]]
	> pad2 m [] nss = nss
	> pad2 m (xs:xss) nss = pad2 m xss ((((amanybs (m - (len xs)) 0) ++ xs)):nss)

	-- ghci> pad [[4,1,1,1],[2,2],[3],[]]

			[[4,1,1,1],
			 [0,0,2,2],
			 [0,0,0,3],
			 [0,0,0,0]]
						 
						 
	-- Reorients a list of lists, like refl did earlier.

	> refactor :: [[Int]] -> [[Int]]
	> refactor [[]] = [[]] 
	> refactor nss  = if sumll nss == 0 
	>   then [[]] else do
	>   let (as, bs) = refactor2 [] [[]] nss 
	>   (reverse as):(refactor (reverse bs))

	> refactor2 :: [Int] -> [[Int]] -> [[Int]] -> ([Int], [[Int]])
	> refactor2 as bss [[]]          = (as,bss)
	> refactor2 as bss ((c:[]):[])   = ((c:as),bss)
	> refactor2 as bss ((c:cs):[])   = ((c:as),(cs:bss))
	> refactor2 as bss ([]:css)      = refactor2 as bss css
	> refactor2 as bss ((c:[]):css)  = refactor2 (c:as) ([]:bss) css
	> refactor2 as bss ((c:cs):css)  = refactor2 (c:as) (cs:bss) css

	-- ghci> refactor [[4,1,1,1],[0,0,2,2],[0,0,0,3],[0,0,0,0]]

			[[4,0,0,0],
			 [1,0,0,0],
			 [1,2,0,0],
			 [1,2,3,0],[]]




	-- The main functions.

	> slvPuzzle2 :: Int -> Int -> IO ()
	> slvPuzzle2 seed n = do
	>   let ((css,rss),grid) = nonoGram seed n
	>   let yo = (refactor (pad css))
	>   putStr (toStr2 (maxlength rss) n ([]:yo))
	>   putStr (printGridS2 (pad rss) (maxlength rss) n grid)				
					
	> printGridS2 :: [[Int]] -> Int -> Int -> [Int] -> String
	> printGridS2 [] _ _ []           = ""
	> printGridS2 [] m n (g:grid)     = (if g == 1 then 'X' else '_'):'|':(printGridS2 [] m n grid)
	> printGridS2 (l:ls) m n (g:grid) = do 
	>   let v = len (g:grid)
	>   let f = if v == n*n then "" else "\n"
	>   let u = if g == 1 then 'X' else '_'
	>   if v `mod` n == 0
	>		then (f ++ (toStr3 l) ++ ('|':u:'|':(printGridS2 ls m n grid)))
	>	    else (u:'|':(printGridS2 (l:ls) m n grid))


	> unsPuzzle2 :: Int -> Int -> IO ()
	> unsPuzzle2 seed n = do
	>   let ((css,rss),grid) = nonoGram seed n
	>   let yo = (refactor (pad css))
	>   putStr (toStr2 (maxlength rss) n ([]:yo))
	>   putStr (printGridU2 (pad rss) (maxlength rss) n grid)				
					
	> printGridU2 :: [[Int]] -> Int -> Int -> [Int] -> String
	> printGridU2 [] _ _ []           = ""
	> printGridU2 [] m n (g:grid)     = '_':'|':(printGridU2 [] m n grid)
	> printGridU2 (l:ls) m n (g:grid) = do 
	>   let v = len (g:grid)
	>   let f = if v == n*n then "" else "\n"
	>   let u = '_'
	>   if v `mod` n == 0
	>		then (f ++ (toStr3 l) ++ ('|':u:'|':(printGridU2 ls m n grid)))
	>	    else (u:'|':(printGridU2 (l:ls) m n grid))



	-- ghci> slvPuzzle2 7239778789 15

		 			       1
		   3   1                     1 2
		   1 4 3   3   2 1   1 2 4 1 1 1
		   1 1 3 1 3 6 2 2   2 1 2 3 2 2
		   1 1 1 3 1 2 2 3 9 2 2 1 1 1 1
		   1 1 2 2 1 2 2 2 3 1 1 4 1 1 1
	 3 1 1 3 1|_|_|X|X|X|_|X|_|X|_|X|X|X|_|X|
	 1 3 1 2 1|X|_|_|_|X|X|X|_|X|_|X|X|_|X|_|
	   3 2 2 1|X|X|X|_|X|X|_|_|X|X|_|X|_|_|_|
	   3 4 1 2|X|X|X|_|_|X|X|X|X|_|_|X|_|X|X|
	   2 3 2 1|_|X|X|_|X|X|X|_|X|X|_|_|_|_|X|
	   2 2 3 2|X|X|_|_|X|X|_|X|X|X|_|_|X|X|_|
    	     2 2 4|_|_|_|_|X|X|_|X|X|_|X|X|X|X|_|
	   3 2 2 1|X|X|X|_|_|_|_|_|X|X|_|X|X|_|X|
		 9|_|_|X|X|X|X|X|X|X|X|X|_|_|_|_|
	   3 3 2 1|_|X|X|X|_|X|X|X|_|_|X|X|_|_|X|
	     1 1 3|_|_|_|X|_|_|_|X|_|_|_|_|X|X|X|
	   1 1 2 1|X|_|X|_|_|_|_|_|X|X|_|X|_|_|_|
   	 1 2 1 2 1|_|_|_|X|_|X|X|_|X|_|X|X|_|_|X|
	     4 4 1|X|X|X|X|_|X|X|X|X|_|_|X|_|_|_|
	   1 1 1 4|_|_|X|_|X|_|_|X|_|_|_|X|X|X|X|
	   
	   
	-- ghci> unsPuzzle2 7239778789 15

					       1
		   3   1                     1 2
		   1 4 3   3   2 1   1 2 4 1 1 1
		   1 1 3 1 3 6 2 2   2 1 2 3 2 2
		   1 1 1 3 1 2 2 3 9 2 2 1 1 1 1
	           1 1 2 2 1 2 2 2 3 1 1 4 1 1 1
	 3 1 1 3 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	 1 3 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   3 2 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   3 4 1 2|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   2 3 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   2 2 3 2|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	     2 2 4|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   3 2 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
		 9|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   3 3 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	     1 1 3|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   1 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	 1 2 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	     4 4 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	   1 1 1 4|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|


	> make :: Int -> Int -> Int -> IO ()
	> make 0 n seed = putStr ""
	> make m n seed = do
	>   unsPuzzle2 seed n
	>   putStr "\n"
	>   slvPuzzle2 seed n
	>   putStr "\n"
	>   make (m-1) n (seed*seed)



	-- ghci> make 3 10 9898989
             2
       2   2 2 2 3 1   1
       1 1 2 1 1 1 1 1 3 1
       2 2 1 1 3 1 1 1 1 1
   1 6|_|_|_|_|_|_|_|_|_|_|
     3|_|_|_|_|_|_|_|_|_|_|
 1 2 2|_|_|_|_|_|_|_|_|_|_|
 1 2 1|_|_|_|_|_|_|_|_|_|_|
 1 2 2|_|_|_|_|_|_|_|_|_|_|
     1|_|_|_|_|_|_|_|_|_|_|
 1 4 1|_|_|_|_|_|_|_|_|_|_|
 1 1 1|_|_|_|_|_|_|_|_|_|_|
   3 1|_|_|_|_|_|_|_|_|_|_|
 1 1 2|_|_|_|_|_|_|_|_|_|_|

             2
       2   2 2 2 3 1   1
       1 1 2 1 1 1 1 1 3 1
       2 2 1 1 3 1 1 1 1 1
   1 6|_|X|_|X|X|X|X|X|X|_|
     3|_|_|_|X|X|X|_|_|_|_|
 1 2 2|_|_|X|_|_|X|X|_|X|X|
 1 2 1|X|_|X|X|_|_|_|_|X|_|
 1 2 2|X|_|_|X|X|_|_|X|X|_|
     1|_|_|X|_|_|_|_|_|_|_|
 1 4 1|X|_|X|X|X|X|_|_|X|_|
 1 1 1|_|X|_|_|X|_|_|_|_|X|
   3 1|X|X|X|_|X|_|_|_|_|_|
 1 1 2|X|_|_|X|_|X|X|_|_|_|

         1
         1   1 1     2   5
         1 2 2 1 1 1 1 2 1 2
         1 2 2 1 3 1 1 1 1 4
   2 1 1|_|_|_|_|_|_|_|_|_|_|
   2 1 4|_|_|_|_|_|_|_|_|_|_|
       5|_|_|_|_|_|_|_|_|_|_|
     1 1|_|_|_|_|_|_|_|_|_|_|
     2 2|_|_|_|_|_|_|_|_|_|_|
 1 1 1 1|_|_|_|_|_|_|_|_|_|_|
     2 1|_|_|_|_|_|_|_|_|_|_|
 1 2 1 2|_|_|_|_|_|_|_|_|_|_|
 1 1 1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|

         1
         1   1 1     2   5
         1 2 2 1 1 1 1 2 1 2
         1 2 2 1 3 1 1 1 1 4
   2 1 1|_|X|X|_|_|X|_|_|X|_|
   2 1 4|X|X|_|X|_|_|X|X|X|X|
       5|_|_|_|_|_|X|X|X|X|X|
     1 1|X|_|_|_|_|_|_|_|X|_|
     2 2|_|_|_|X|X|_|_|X|X|_|
 1 1 1 1|X|_|X|_|_|_|X|_|_|X|
     2 1|_|X|X|_|_|_|_|_|_|X|
 1 2 1 2|_|X|_|X|X|_|X|_|X|X|
 1 1 1 1|X|_|X|_|X|_|_|_|_|X|
   1 1 1|_|_|X|_|X|_|_|_|X|_|

                 1 1     1
         3 1 1   2 1 1 1 1 4
         2 2 1 3 1 1 1 2 1 2
         1 3 2 2 1 1 1 2 1 1
     2 6|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|
   5 1 1|_|_|_|_|_|_|_|_|_|_|
 1 2 1 1|_|_|_|_|_|_|_|_|_|_|
   1 2 3|_|_|_|_|_|_|_|_|_|_|
     1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 3|_|_|_|_|_|_|_|_|_|_|
     2 1|_|_|_|_|_|_|_|_|_|_|
 1 1 3 2|_|_|_|_|_|_|_|_|_|_|

                 1 1     1
         3 1 1   2 1 1 1 1 4
         2 2 1 3 1 1 1 2 1 2
         1 3 2 2 1 1 1 2 1 1
     2 6|X|X|_|X|X|X|X|X|X|_|
   1 1 1|X|_|_|X|_|_|_|_|_|X|
   5 1 1|X|X|X|X|X|_|X|_|_|X|
 1 2 1 1|_|X|_|_|X|X|_|X|_|X|
   1 2 3|X|_|X|X|_|_|_|X|X|X|
     1 1|X|_|_|X|_|_|_|_|_|_|
   1 1 1|_|X|_|_|X|_|_|_|_|X|
   1 1 3|_|X|_|_|_|X|_|X|X|X|
     2 1|_|X|X|_|_|_|_|X|_|_|
 1 1 3 2|X|_|X|_|X|X|X|_|X|X|


rng

In this section we will be exploring randomness. We will start by defining 
permuting and sorting, generate some pseudo-random numbers, glance at some 
quantum voodoo, and end up with a pleasant little game.



    Merge Sort

> mergeSort :: [Integer] -> [Integer]
> mergeSort      [] = []
> mergeSort     [x] = [x]
> mergeSort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> mergeSort list    = collate (mergeSort a) (mergeSort b)
>      where (a, b) = splitAt ((length(list)) `div` 2) list

If the list has three or more elements, we split it, sort both, then collate. Haskell integer division is such that, if the list has an odd number of elements, the first will be larger.

> collate :: [Integer] -> [Integer] -> [Integer]
> collate        as []  = as
> collate (a:as) (b:bs) = if (a>b) then b:collate (a:as) bs 
>                                  else a:collate as (b:bs)

These two lists are sorted, so either a or b will be our smallest element. So, we will collate once for each list element.

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

We pivot about the middle element, calling a new function that will cleave the list into two: one less, one greater, and put the two into that empty list we give it. We then call quickSort again, on both halves, ordering them correctly.

> pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]

These two algorithms are both n(log(n))ish, as they both work by splitting the initial lists in two as many times as necessary. I used the below function to test them.

> list1ton :: Integer -> [Integer]
> list1ton n | n<1 = []
> list1ton n = n:(list1ton (n-1))



The most common algorithmic (i.e. fake) random number generator goes by the 
longname 

Linear Congruential Generator. 

The basic equation is x_2 = (a*x_1 + c) % m.
 
  The game is to make m as big as possible, and 
  ensure it's got no factors other than two.

    You should make c prime, ideally, or at the 
    very least coprime to m.

    Then, if you ensure these two things, the 
    resulting sequence will only repeat once 
    every m times:

      1. a-1 `div` f == 0 for all prime factors f of m
      2. a-1 `mod` 4 == 1 if m `div` 4 == 0

These particular parameters are Word of Knuth.


> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))


The first pseudorandom number generator comes courtesy of noted fed John Von 
Neumann. The meat of his method is an iterative process, but one that eventually 
tends to zero. Not cool Johnny! He squares a seed number with x many digits, rips 
out x many digits from the middle, squares that, yada yada. 

> lenn :: Int -> Int
> lenn i = if i<1 then 0 else 1 + (lenn (i `div` 10))

> inst2 :: Int -> Int
> inst2 0        =  0
> inst2 r        =  do
>   let rr       =  r*r
>   let (lr,lrr) = (lenn r,lenn rr)
>   let x        = (lrr - lr) `div` 2
>   (rr `div` (10^x)) `mod` (10^lr)

> msm :: Int -> Int -> [Int]
> msm ornd 1 = [(inst2 ornd)]
> msm ornd it = do 
>   let nrnd = inst2 ornd 
>   if nrnd == ornd then [ornd] else (nrnd : msm nrnd (it-1))

In this case, the middle square method peters out after 88 iterations, while 
the linear congruential method carries on unfazed.

ghci> lcg 314159265 100
    [1811288460,2039979899,1706036894, ..., 2887701979,630722942,1903037125]
ghci> msm 314159265 100
    [604378534,341235998,200633105, ... 7746,5,5]

In general, we will use the linear congruential method because of its speed 
and effectiveness. Here are some scattered other methods:

Lehmer RNG 

This is an LCG with one term removed. It's better than MSM. The two numbers 
should be coprime.

> inst3 :: Int -> Int
> inst3 r = (16807*r) `mod` ((2 ^ 31)-1) 

> lrng :: Int -> Int -> [Int]
> lrng ornd 1  = [(inst3 ornd)]
> lrng ornd it = do 
>   let nrnd = inst3 ornd 
>   (nrnd : lrng nrnd (it-1))

For our next method to work we will need to talk about bitwise operations on
integers. A bit is a zero or a one. Using them, we can write any number in base 
two, like 

1    7    3  =  173_10 = 10101101_2 = 1   0   1   0   1   1   0   1

100  10   1                           128 64  32  16  8   4   2   1 

10^2 10^1 10^0                        2^7 2^6 2^5 2^4 2^3 2^2 2^1 2^0

A binary operation is something like plus or divide, that takes in two things 
and outputs one. There are 16 possible binary operations on bits. I will present 
three, and code one.

OR AND XOR

  AND   \    OR   \     XOR
0&0 = 0 \ 0|0 = 0 \ 0~0 = 0 
0&1 = 1 \ 0|1 = 1 \ 0~1 = 1 
1&0 = 1 \ 1|0 = 1 \ 1~0 = 1 
1&1 = 1 \ 1|1 = 1 \ 1~1 = 1 

We can do each of these operations on two full integers, by applying the operation 
to each bit in the binary representation of the numbers. Thus, 

14 ~ 11 = 1110 ~
          1011 = 
          0101_2 = 5_10

> (~) :: Int -> Int -> Int
> (~) a 0 = a
> (~) 0 b = b
> (~) a b = do
>   let (da,ma) = a `divMod` 2
>   let (db,mb) = b `divMod` 2
>   if ma == mb 
>     then 2*(da ~ db)
>     else 1+(2*(da ~ db))

It is simple to code similar operations for OR and AND. With XOR, we can create 
a Lagged Fibonacci Generator.

The root equation is x_n = (x_(n-j) <op> x_(n-k)) % m

m is usually 2^2^x where x is something or other.
 
y = x^k + x^j + 1 needs to be irreducible (unfactorable) and primitive mod 2.

Here are some (j,k)-tuples satisfying that condition, taken from page 29 of 
volume 2 of The Art of Computer Programming:

(24, 55),  (38, 89),   (37, 100),  (30, 127), 
(83, 258), (107, 378), (273, 607), (1029, 2281), 
(576, 3217), (4187, 9689), (7083, 19937), (9739, 23209)

(5,17) (6,31) and (7,10) also work, but that's not coming from Knuth.

> lfg :: Int -> Int -> [Int]
> lfg ornd it = lfg2 (lcg ornd 55) it

> lfg2 :: [Int] -> Int -> [Int] 
> lfg2 seeds 0 = []
> lfg2 seeds i = do 
>   let nrnd   = ((seeds !! ((len seeds)-24)) ~ (head seeds)) `mod` (2^32)
>   (nrnd : lfg2 (nrnd : take 54 seeds) (i-1))

Lastly, let us talk about Elementary Cellular Automata. These are iterative 
processes on a line of bits (which, if you remember, can be construed into 
regular numbers). Whether a bit becomes a one or zero is determined by its 
neighbors. In one dimension, there are three decisive actors: the left neighbor, 
right neighbor, and the cell itself. There re eight ways these can be arranged, 
so there are 256 possible rules. One of them, Rule 30, is notably chaotic, and
generates random numbers well.

Rule 30

30 in base 10 = 00011110 in base 2

111 110 101 100 011 010 001 000
 0   0   0   1   1   1   1   0

> inst4 :: Int -> Int
> inst4 0 = 0
> inst4 r = do 
>   let w = r `mod` 8 
>   if 0 < w && w < 5
>     then 1 + (2*(inst4 (r `div` 2)))
>     else     (2*(inst4 (r `div` 2)))


> rule30 :: Int -> Int -> [Int]
> rule30 ornd 1  = [(inst4 ornd)]
> rule30 ornd it = do 
>   let nrnd = inst4 ornd 
>   (nrnd : lcg nrnd (it-1))




TODO:
	INVERSIVE CONGRUENTIAL GENERATOR
	BLUM BLUM SHUB
	ACORN
	MIXMAX
	KISS
	MERSENNE TWISTER
	XORSHIFT
	LINEAR-FEEDBACK SHIFT REGISTER

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



































bas

In this section we will be exploring bases. We will take integers as a given, 
and explore rational and real representations like floating-point and continued
fractions. We'll program in alternate bases, touch on p-adics, and ultimately wind
up at the Basel problem.


When one divides by three, the result repeats infinitely. This is unfathomable. What will we do.

It is ok. It is just periodic. All rational numbers have an infinite, but periodic decimal, such as 

4/1  = 4.(0 repeating)

1/7  = 0.(142857 repeating)

1/3  = 0.(3 ...)

3/12 = 0.375(0 ...)

and so on, using the ellipses to mean "what precedes repeats forever". 

One type of number that is not rational is called an irrational. An example is the square root of five, which goes something like 

2.2360679774997896964091736687312762354406183596115257242708972454105209256378048994144144083787822749695081761507737835042532677244470738635863601215334527088667781731918791658112766453226398565805357613504175337850034233924140644420864325390972525926272288762995174024406816117759089094984923713907297288984820886415426898940991316935770197486788844250897541329561831769214999774248015304341150359576683325124988151781394080005624208552435422355561063063428202340933319829339597463522712013417496142026359047378855043896870611356600457571399565955669569175645782219525000605392312340050092867648755297220567662536660744858535052623306784946334222423176372770266324076801044433158257335058930981362263431986864719469899701808189524264459620345221411922329125981963258111041704958070481204034559949435068555518555725123886416550102624363125710244496187894246829034044747161154557232017376765904609185295756035779843980541553807790643936397230287560629994822138521773485924535151210463455550407072278724215347787529112121211843317893351910380080111181790045906188462496471042442483088801294068113146959532794478989989316915774607924618075006798771242048473805027736082915599139624489149435606834625290644083279446426808889897460463083535378750420613747576068834018790881925591179735744641902485378711461940901919136880351103976384360412810581103786989518520146970456420217638928908844463778263858937924400460288754053984601560617052236150903857754100421936849872542718503752155576933167230047782698666624462106784642724863852745782134100679856453052711241805959728494551954513101723097508714965294362829025400120477803241554644899887061779981900336065622438864096392877535172662959714382279563079561495230154442350165389172786409130419793971113562821393674576811749220675621088878188736716716276226233798771115395096829828906830182590814010038955097232615084528345878936073463961172366783665719826079214402891190089955842415224957129183232167411899757201394037881977280152887234186683454183828673002743153202296076286125247610286423469630201118026912202360158101276284305418617176...

And will say the same sort of thing in any rational base. So here's the issue: we cannot represent this style of number as a predictable pattern of integers. This is the big idea:


   415 / 93 = 4 +          1
				  ______________________
					 2    +    1
						 _______________
						  6 +      1
								________
								   7 

All rational numbers have a finite representation along these lines, as a continued fraction. Starting with any rational, we can apply Euclid's GCD algorithm, like 

415 /%/ 93 = (4, 43) 
 93 /%/ 43 = (2, 7) 
 43 /%/  7 = (6, 1) 
  7 /%/  1 = (7, 0) 
  
Basically just long division

	 4
   ___________
93|415        
  -372 = 43

	  
  
  
getting us all of the information we'd need to fill in a template version of what's up there. This is nice: absolute precision for all rational numbers, and one canonical description per rational. In the case of 
 
  415  
 _____  =  4.(462365591397849 ...) we could store the number merely as [4,2,6,7]. 
  93

> (/%/) :: Integer -> Integer -> (Integer, Integer)
> a /%/ b = a `divMod` b
 
> rationalToCF :: Integer -> Integer -> [Integer]
> rationalToCF a 0 = [] 
> rationalToCF a b = do 
>   let (a',b') = a /%/ b
>   a':(rationalToCF b b')

ghci> rationalToCF 415 93 = [4,2,6,7]

ghci> rationalToCF 7487 12365 = [0,1,1,1,1,6,1,2,15,1,6]

This is cool for numbers whose exact numerator and denominator we know, but how about decimals where we don't know? Uhh, let's check https://www.math.u-bordeaux.fr/~pjaming/M1/exposes/MA2.pdf.



 
There are plenty of reasons that this trick works, one of which is that the harmonic series 
diverges. Anyway, with these, we can apply Euclid's cracked/jailbroke incommensurable variant 
of the GCD algorithm, and depict irrational numbers in another way, which also repeats forever.
Now, though, some repeat predictably, like 


19 ^ 1/2 = [4;(2,1,3,1,2,8 ...)]

e        = [2;1,2,1,1,4,1,1,(p+2,1,1 ...)]

2  ^ 1/2 = [1;(2 ...)]

phi      = [1 ...]

In general, any periodic continued fraction represents a root of a quadratic equation with integer coefficients (if we count the rationals repeating zero forever, and acknowledge that e is there on a technicality).

Floating Point is not optimized for numbers like square roots and e, despite those are pretty 
common I think. It is also not sorted to where storing integers is pretty much as good as a 
normal integer, in terms of arithmetic error and storage space. 

If I may engage the reader emotionally, I recently watched a lovely Youtube personality spend 
an hour doing really cool stuff with the Collatz conjecture, writing some algorithms to fetch 
arbitrary configurations from the Tree. They were in Python, and as such, floating point error 
ended the project once numbers got sufficiently large. I'm not against decimal values: we could
linearly interpolate "2x" and "3x+1" by the real number mod 2 (i.e., 3.67 % 2 = 1.67),turning "even" and "odd" to poles along a spectrum, but that's a whole other thing. 

Uhh where was I uhh

oh thiss is dope

43/19 = [2;3,1,4]   |   19/43 = [0;2,3,1,4]
 3/7  = [0;2,3]     |    7/3  = [2;3]

Esp. when you consider that "rolling" a rational continued fraction back up into an over b 
style situation involves negative first powers and adding 1, two exceedingly simple operations (to add one add one to first integer in list).


 
Here's that same operation for negation, which isn't quite as simple. 

 17/12 = [ 1;2,2,2]   |  5/8  = [0;1,1,1,2]
-17/12 = [-2;1,1,2,2] | -5/8 = [-1;]



   x = [   a;b,   c,d,e,f,   etc.]
  -x = [-1-a;1,-1+b,c,d,e,f, etc.]
 
  
 100 = [100]
-100 = [-101;1,-1] = -100 

back-compatible!


 5/3 = [ 1;1,2]
-5/3 = [-2;1,0,2] = [-2; 3]

We can remove zeroes by, if they're in the middle, adding the numbers before and after it together. 

Some cool facts:

	All negative numbers can be written with totally positive numbers after the semicolon
	
	You don't want negative numbers after the semicolon, because they allow for infinitely many
	ways of writing zero, such as [1,-1], which makes deriving values more complex than it 
	needs to be.
	
	Some periodic infinite fractions with negative post-semicolon terms diverge, but all 
	positive ones don't

So we can pretty much ignore negatives. 


This gives unique representations, like decimal, pure integer representations for integers (as 
a list of integers with one integer in it), and lightweight representations of many widely used
numbers. 

Euler's identity gives us the trig expansion pack, at the expense of forcing us to use the general form, without unit numerators.



e^x =   1 +             x
			__________________________
			1 -             x
				______________________
				2+x-         2x
					 _________________
					 3+x-      3x
						  ____________
						  4+x-   ...
						  
log x = 1 +             x
			__________________________
			1 +             x
				______________________
				2-x+         4x
					 _________________
					 3-2x+      9x
						  ____________
						  4-3x+   ...
						 
With both numerators and denominators, we can write z^(m/n)	for any integer z,m,n as a periodic
continued fraction.					 

https://wikimedia.org/api/rest_v1/media/math/render/svg/68ca4eebd9f9f51d6a7ce01affd6e4288c645997

https://perl.plover.com/yak/cftalk/INFO/gosper.txt

https://pi.math.cornell.edu/~gautam/ContinuedFractions.pdf

All real numbers have some continued fraction, but ones like pi are random. 


1,2,5,12,29,70,169,408

2 cos(pi/5)



> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y

> isPrime :: Int -> Bool 
> isPrime p = if p `mod` 2 == 0 then False 
>               else isPrime2 p 3 (sq p) 

> isPrime2 :: Int -> Int -> Int -> Bool 
> isPrime2 p i n = if i>n  then True 
>   else if p `mod` i == 0 then False 
>                          else isPrime2 p (i+2) n

> nextPrime :: Int -> Int  
> nextPrime n = if isPrime n then n else nextPrime (n+1)

> prevPrime :: Int -> Int  
> prevPrime n = if isPrime n then n else prevPrime (n-1)

> tallyPrimesQuick :: Int -> [Int]
> tallyPrimesQuick n = if n<2 then [] 
>                     else if isPrime n 
>                       then n:(tallyPrimesQuick (n-1)) 
>                       else tallyPrimesQuick (n-1)

> coprime2 :: Int -> [Int] -> Bool
> coprime2 _ []     = True
> coprime2 a (p:ps) = if a `mod` p == 0 then False else coprime2 a ps

> cutoff :: Int -> [Int] -> [Int] 
> cutoff n []     = []
> cutoff n (a:as) = if n>a then [] else a:(cutoff n as)

> tallyPrimesSlow :: Int -> [Int]
> tallyPrimesSlow n = tallyPrimesSlow2 2 n []

> tallyPrimesSlow2 :: Int -> Int -> [Int] -> [Int]
> tallyPrimesSlow2 i n ps = if i>n then ps 
>                            else if coprime2 i (cutoff (sq i) ps) 
>                                   then tallyPrimesSlow2 (i+1) n (i:ps) 
>                                   else tallyPrimesSlow2 (i+1) n (ps)

-- With this I can confidently state

> primesto10000 :: [Int]
> primesto10000 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997,1009,1013,1019,1021,1031,1033,1039,1049,1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,1129,1151,1153,1163,1171,1181,1187,1193,1201,1213,1217,1223,1229,1231,1237,1249,1259,1277,1279,1283,1289,1291,1297,1301,1303,1307,1319,1321,1327,1361,1367,1373,1381,1399,1409,1423,1427,1429,1433,1439,1447,1451,1453,1459,1471,1481,1483,1487,1489,1493,1499,1511,1523,1531,1543,1549,1553,1559,1567,1571,1579,1583,1597,1601,1607,1609,1613,1619,1621,1627,1637,1657,1663,1667,1669,1693,1697,1699,1709,1721,1723,1733,1741,1747,1753,1759,1777,1783,1787,1789,1801,1811,1823,1831,1847,1861,1867,1871,1873,1877,1879,1889,1901,1907,1913,1931,1933,1949,1951,1973,1979,1987,1993,1997,1999,2003,2011,2017,2027,2029,2039,2053,2063,2069,2081,2083,2087,2089,2099,2111,2113,2129,2131,2137,2141,2143,2153,2161,2179,2203,2207,2213,2221,2237,2239,2243,2251,2267,2269,2273,2281,2287,2293,2297,2309,2311,2333,2339,2341,2347,2351,2357,2371,2377,2381,2383,2389,2393,2399,2411,2417,2423,2437,2441,2447,2459,2467,2473,2477,2503,2521,2531,2539,2543,2549,2551,2557,2579,2591,2593,2609,2617,2621,2633,2647,2657,2659,2663,2671,2677,2683,2687,2689,2693,2699,2707,2711,2713,2719,2729,2731,2741,2749,2753,2767,2777,2789,2791,2797,2801,2803,2819,2833,2837,2843,2851,2857,2861,2879,2887,2897,2903,2909,2917,2927,2939,2953,2957,2963,2969,2971,2999,3001,3011,3019,3023,3037,3041,3049,3061,3067,3079,3083,3089,3109,3119,3121,3137,3163,3167,3169,3181,3187,3191,3203,3209,3217,3221,3229,3251,3253,3257,3259,3271,3299,3301,3307,3313,3319,3323,3329,3331,3343,3347,3359,3361,3371,3373,3389,3391,3407,3413,3433,3449,3457,3461,3463,3467,3469,3491,3499,3511,3517,3527,3529,3533,3539,3541,3547,3557,3559,3571,3581,3583,3593,3607,3613,3617,3623,3631,3637,3643,3659,3671,3673,3677,3691,3697,3701,3709,3719,3727,3733,3739,3761,3767,3769,3779,3793,3797,3803,3821,3823,3833,3847,3851,3853,3863,3877,3881,3889,3907,3911,3917,3919,3923,3929,3931,3943,3947,3967,3989,4001,4003,4007,4013,4019,4021,4027,4049,4051,4057,4073,4079,4091,4093,4099,4111,4127,4129,4133,4139,4153,4157,4159,4177,4201,4211,4217,4219,4229,4231,4241,4243,4253,4259,4261,4271,4273,4283,4289,4297,4327,4337,4339,4349,4357,4363,4373,4391,4397,4409,4421,4423,4441,4447,4451,4457,4463,4481,4483,4493,4507,4513,4517,4519,4523,4547,4549,4561,4567,4583,4591,4597,4603,4621,4637,4639,4643,4649,4651,4657,4663,4673,4679,4691,4703,4721,4723,4729,4733,4751,4759,4783,4787,4789,4793,4799,4801,4813,4817,4831,4861,4871,4877,4889,4903,4909,4919,4931,4933,4937,4943,4951,4957,4967,4969,4973,4987,4993,4999,5003,5009,5011,5021,5023,5039,5051,5059,5077,5081,5087,5099,5101,5107,5113,5119,5147,5153,5167,5171,5179,5189,5197,5209,5227,5231,5233,5237,5261,5273,5279,5281,5297,5303,5309,5323,5333,5347,5351,5381,5387,5393,5399,5407,5413,5417,5419,5431,5437,5441,5443,5449,5471,5477,5479,5483,5501,5503,5507,5519,5521,5527,5531,5557,5563,5569,5573,5581,5591,5623,5639,5641,5647,5651,5653,5657,5659,5669,5683,5689,5693,5701,5711,5717,5737,5741,5743,5749,5779,5783,5791,5801,5807,5813,5821,5827,5839,5843,5849,5851,5857,5861,5867,5869,5879,5881,5897,5903,5923,5927,5939,5953,5981,5987,6007,6011,6029,6037,6043,6047,6053,6067,6073,6079,6089,6091,6101,6113,6121,6131,6133,6143,6151,6163,6173,6197,6199,6203,6211,6217,6221,6229,6247,6257,6263,6269,6271,6277,6287,6299,6301,6311,6317,6323,6329,6337,6343,6353,6359,6361,6367,6373,6379,6389,6397,6421,6427,6449,6451,6469,6473,6481,6491,6521,6529,6547,6551,6553,6563,6569,6571,6577,6581,6599,6607,6619,6637,6653,6659,6661,6673,6679,6689,6691,6701,6703,6709,6719,6733,6737,6761,6763,6779,6781,6791,6793,6803,6823,6827,6829,6833,6841,6857,6863,6869,6871,6883,6899,6907,6911,6917,6947,6949,6959,6961,6967,6971,6977,6983,6991,6997,7001,7013,7019,7027,7039,7043,7057,7069,7079,7103,7109,7121,7127,7129,7151,7159,7177,7187,7193,7207,7211,7213,7219,7229,7237,7243,7247,7253,7283,7297,7307,7309,7321,7331,7333,7349,7351,7369,7393,7411,7417,7433,7451,7457,7459,7477,7481,7487,7489,7499,7507,7517,7523,7529,7537,7541,7547,7549,7559,7561,7573,7577,7583,7589,7591,7603,7607,7621,7639,7643,7649,7669,7673,7681,7687,7691,7699,7703,7717,7723,7727,7741,7753,7757,7759,7789,7793,7817,7823,7829,7841,7853,7867,7873,7877,7879,7883,7901,7907,7919,7927,7933,7937,7949,7951,7963,7993,8009,8011,8017,8039,8053,8059,8069,8081,8087,8089,8093,8101,8111,8117,8123,8147,8161,8167,8171,8179,8191,8209,8219,8221,8231,8233,8237,8243,8263,8269,8273,8287,8291,8293,8297,8311,8317,8329,8353,8363,8369,8377,8387,8389,8419,8423,8429,8431,8443,8447,8461,8467,8501,8513,8521,8527,8537,8539,8543,8563,8573,8581,8597,8599,8609,8623,8627,8629,8641,8647,8663,8669,8677,8681,8689,8693,8699,8707,8713,8719,8731,8737,8741,8747,8753,8761,8779,8783,8803,8807,8819,8821,8831,8837,8839,8849,8861,8863,8867,8887,8893,8923,8929,8933,8941,8951,8963,8969,8971,8999,9001,9007,9011,9013,9029,9041,9043,9049,9059,9067,9091,9103,9109,9127,9133,9137,9151,9157,9161,9173,9181,9187,9199,9203,9209,9221,9227,9239,9241,9257,9277,9281,9283,9293,9311,9319,9323,9337,9341,9343,9349,9371,9377,9391,9397,9403,9413,9419,9421,9431,9433,9437,9439,9461,9463,9467,9473,9479,9491,9497,9511,9521,9533,9539,9547,9551,9587,9601,9613,9619,9623,9629,9631,9643,9649,9661,9677,9679,9689,9697,9719,9721,9733,9739,9743,9749,9767,9769,9781,9787,9791,9803,9811,9817,9829,9833,9839,9851,9857,9859,9871,9883,9887,9901,9907,9923,9929,9931,9941,9949,9967,9973]


> pcf :: Int -> Int
> pcf a = if a<2 then 0 else if isPrime a then 1+(pcf (a-1)) else pcf (a-1)

-- mm.. quite slow, maybe that list could help.

> pcf1 :: Int -> Int
> pcf1 n = pcf2 n primesto10000

> pcf2 :: Int -> [Int] -> Int
> pcf2 n []     = pcf3 10000 n
> pcf2 n (p:ps) = if n<p then 0 else 1+(pcf2 n ps)

> pcf3 :: Int -> Int -> Int
> pcf3 i n = if i>n then 0 else if isPrime i then 1+(pcf3 (i+1) n) else pcf3 (i+1) n

ghci> totient 15000
1754
ghci> 15000 / (log 15000)
1559.931721899643

> gcd' :: Int -> Int -> Int
> gcd' a 0 = a 
> gcd' a b = gcd' b (a `mod` b)

> lcm' :: Int -> Int -> Int
> lcm' a b = (a*b) `div` (gcd' a b)

> primeFactors :: Int -> [Int]
> primeFactors a = factors2 a primesto10000

> factors2 :: Int -> [Int] -> [Int]
> factors2 a []     = []   
> factors2 a (p:ps) = if a<2 then [] 
>   else if isPrime a then [a] 
>   else do 
>     let (da,ma) = a `divMod` p
>     if ma == 0 then p:(factors2 da (p:ps))
>                else factors2 a ps

-- How they get factors in the user's guide (slower and not all prime):

> divisors :: Int -> [Int]
> divisors n = [d | d <- [1..(n `div` 2)], n `mod` d == 0]


> coprime :: Int -> Int -> Bool 
> coprime a b = if gcd a b == 1 then True else False

> totient :: Int -> Int 
> totient n = totient2 1 n

> totient2 :: Int -> Int -> Int 
> totient2 i n = if i>n then 0 
>           else if gcd i n == 1 
>                  then 1+(totient2 (i+1) n) 
>                  else    totient2 (i+1) n


> (~) :: Int -> Int -> Int
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))

> (&) :: Int -> Int -> Int
> a & 0 = a
> 0 & b = b
> a & b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb && ma == 1
>     then 1+(2*(da & db))
>     else    2*(da & db)

> (#) :: Int -> Int -> Int
> a # 0 = a
> 0 # b = b
> a # b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb && ma == 1
>     then 1+(2*(da # db))
>     else    2*(da # db)

> (?) :: Int -> Int -> [Int]
> n ? seed = lcg seed n

1
0 1
0 1 1
0 1 3  1
0 1 7  6  1
0 1 15 25 10 1

> stir :: Int -> Int -> Int
> stir n k =    if n==k then 1 
>   else if k==0 || k>n then 0 
>   else (k*(stir (n-1) k)) + (stir (n-1) (k-1)) 







-- The central limit theorem means we can also just do an Irwin-Hall cop-out.

normal :: Double -> Double -> Integer -> Double 
normal m s2 seed = do 
  let rs12 = rng seed 12 10000
  (((realToFrac (sum rs12)) / 10000) - 6)*(s2**0.5) + m

















	-------------------------------------------------------------------------

	-- Not worth explaining

	> toStr :: [Int] -> String
	> toStr []     = ""
	> toStr (a:as) = (if a < 10 then ' ':(show a) else show a) ++ toStr as 

	> toStr2 :: Int -> Int -> [[Int]] -> String
	> toStr2 _ _ ([]:[[]])      = "\n"
	> toStr2 a n ([]:wss)       = "\n" ++ (rep a "  ") ++ toStr2 a n wss
	> toStr2 a n ((0:ws):wss)   = (' ':' ':(toStr2 a n (ws:wss)))
	> toStr2 a n ((w:ws):wss)   = (' ':(show w)) ++ toStr2 a n (ws:wss)

	> toStr3 :: [Int] -> String
	> toStr3 []     = ""
	> toStr3 (a:as) = if a==0 
	>					then ' ':' ':toStr3 as 
	>					else ((if a < 10 
	>							then " " ++ (show a) 
	>							else show a) ++ toStr3 as)


	> rep :: Int -> [a] -> [a]
	> rep a bs = rep2 bs a []

	> rep2 :: [a] -> Int -> [a] -> [a]
	> rep2 _  0 bs = bs
	> rep2 as i bs = as++(rep2 as (i-1) bs)

	> maxlength :: [[a]] -> Int
	> maxlength [[]] = 0
	> maxlength [as] = len as
	> maxlength (as:bs:css) = do 
	>   let la = len as
	>   let lb = len bs
	>   if (la>lb) then maxlength (as:css) else maxlength (bs:css) 

	> extract :: Int -> [Int] -> [Int] 
	> extract 0 as     = []
	> extract n (a:as) = a:(extract (n-1) as)

	> retract :: Int -> [Int] -> [Int] 
	> retract 0 as     = as
	> retract n (a:as) = retract (n-1) as

	> amanybs :: Int -> a -> [a]
	> amanybs 0 b = []
	> amanybs a b = (b:amanybs (a-1) b)

	> sumll :: [[Int]] -> Int
	> sumll [[]]         = 0
	> sumll ([]:qs)      = sumll qs
	> sumll ((q:qs):qss) = q + sumll (qs:qss)



	> sq :: Int -> Int
	> sq n = sqrt2 0 n

	> sqrt2 :: Int -> Int -> Int
	> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y




 interpQLit x y (Shape (Ply   n r a b)) = if (((cosine (3.14159265359 / n))/(cosine ((phase   (x :+ y)) - (((2*3.14159265359)/n)*
    (realToFrac (floor (((n*(phase   (x :+ y)))+3.14159265359)/(3.14159265359*2)))))))) - (((x-a)*(x-a)) + ((y-b)*(y-b)))) < 0.0 then 1.0 else 0.0


        > partitionHelper0 :: Integer -> Integer -> Integer
        > partitionHelper0 i c = if (i<0) then 0 else ((partition i) + (partitionHelper1 (i - 3 - c) (c+1)))
        >
        > partitionHelper1 :: Integer -> Integer -> Integer
        > partitionHelper1 i c = if (i<0) then 0 else ((partition i) - (partitionHelper2 (i - 5 - (2*c)) (c)))
        >
        > partitionHelper2 :: Integer -> Integer -> Integer
        > partitionHelper2 i c = if (i<0) then 0 else ((partition i) - (partitionHelper3 (i - 3 - c) (c+1)))
        >
        > partitionHelper3 :: Integer -> Integer -> Integer
        > partitionHelper3 i c = if (i<0) then 0 else ((partition i) + (partitionHelper0 (i - 5 - (2*c)) (c)))

-- I believe that a woman (or man) should find an adequate sorting 
-- algorithm in her teens and allow it to be a meditative act, with 
-- no further analysis.















> data Tiling where
>   Tri         :: Tiling
>   Squ         :: Tiling
>   Hex         :: Tiling
>   T_3_12'2    :: Tiling
>   T_3_4_6_4   :: Tiling
>   T_4_6_12    :: Tiling
>   T_3_6_3_6   :: Tiling
>   T_4_8'2     :: Tiling
>   T_4_3_4_3'2 :: Tiling
>   T_3'3_4'2   :: Tiling
>   T_6_3'4     :: Tiling












o GROUP CAN BE [BROKEN LINE, 
                UNBROKEN LINE, 
				LINE OF SIGHT, 
				CONTIGREGION, 
				SHAPE(TURTLE STEP NOTATION, W HIPPOGONALITY AND X Y Z TYPE NOTATION FORMULATED FOR ENTROPY), 
				LOOP(CAPTURE,SIDES,EDGEALIGNMENT), 
				CONNECT {X, Y}]







  










BOARD (CAN HAVE ONE OR SEVERAL)

O EACH PLAYER AUTO HAS A SQUARE, HEX, OR CIRCLE WITH THEIR CHARACTER ON IT. THIS IS USED TO REPRESENT TURN ORDER, CAN BE ANY WHOLE NUMBER. CAN BE REFERRED TO AS P1, P2, ALL, ANY, ME, NEXT, PREV, RAND OR OTHER.

O [GRID, HEX, GRAPH]
	MAY DELETE INDIV. NODES MANUALLY, BUT UP TOP MAY ONLY MAKE A SQUARE OR RHOMBUS W GRIDS.

	O [UNBOUND, BOUND, 2-TORUS, 4-TORUS]
	
	O [LIST OF PLAYERS] EACH SITE CAN BE SET VISIBLE TO ALL, NONE, OR A LIST OF PLAYERS

INIT

o CONFIGURE UNITS ON BOARD MANUALLY. 
  STACKS POSSIBLE, HAND AND BOARD LOOK SAME.

O UNITS CAN HAVE ONE OF 12 COLORS, AND ONE OF 48 CHARACTERS. BOOLEANS CAN CHECK BOTH.

o CAN SET RANDOM PERMUTATION OF GIVEN ELEMENTS, 
  IN STACK (CARDS) OR PARTITION ACROSS BOARD (LIKE CHESS 960)

o CAN DEFINE RANDOM DISTRIBUTIONS FOR DICE
  SET INIT LISTS/VARIABLES IN THIS PART
  LISTS CAN'T BE CREATED MIDGAME, SO MAYBE DYNAMIC VARIABLES, SO YOU CAN UPDATE A LIST BY UPDATING A VARIABLE WITHIN IT?
o GROUP CAN BE [BROKEN LINE, 
                UNBROKEN LINE, 
				LINE OF SIGHT, 
				CONTIGREGION, 
				SHAPE(TURTLE STEP NOTATION, W HIPPOGONALITY AND X Y Z TYPE NOTATION FORMULATED FOR ENTROPY), 
				LOOP(CAPTURE,SIDES,EDGEALIGNMENT), 
				CONNECT {X, Y}]
o KOMI/PIE
o LISTS ARE LITERALLY BOARDSPACE, CAN HOLD CHARS=STRING OR INTS, VERT INDENT IS A COMMA.
o NO NEW UNITS MADE IN GAME
o AUTODICT "TERRITORY" PLAYERS TO LISTS OF UNITS ON BOARD


FROM:
	[LIST OF BOARDSPACES, 
	"MYBOARDSPACE", 
	"YOURBOARDSPACE", 
	LAST TO [PX], 
	A COORDINATE, 
	A LIST OF COORDINATES.] 
	
	[PARTIC UNIT, OPTIONAL]

TO:
	LIST OF BOARDSPACES
	"EMPTY" 
	"MYBOARDSPACE" 
	"YOURBOARDSPACE" 
	A COORDINATE 
	OR A LIST OF COORDINATES
	(TURTLE STEP)
	
	[PARTIC UNIT, OPTIONAL] 
	[CONDITIONAL BOOLEAN (GROUP SANCTITY)]




PLAY

o PHASE {

	[IF[BOOL], ELSE[BOOL], LOOP[ALL,ANY,N]]

		[PASS]
		
		[MOVE[FROM, TO]]
	
		[REMOVE[FROM]]

		[LET [STR] = [INT,STR,LIST]]

		[DEF [a -> a]]
		
		[VISIBILITY TOGGLE]
		
		[SWITCH[REGION]]
		
		[TRANSFORM[FROM, TO]]


	[PASS, MOVE[FROM,TO], ADD[BY CHOICE OR BY ALGORITHM], REMOVE, LOOP[ALL,ANY,N], LET [STR] =, VISIBILITYTOGGLE]
	[MOVE + MOVE = SWITCH, CAN BE OVERLOADED TO RANDOMLY PERMUTE]
	[REMOVE + ADD = TRANSFORM]
	[HET STACK INHERENT]
	[ALL TO A UNIT OR GROUP]
	[BOOL, GROUP] GROUP SANCTITY
}

WIN

FORM N GROUP(S)
UNIT IN TERRITORY
REMOVE ALL UNITS OF TYPE T
POINT TALLY (NEEDS (PCOUNT) VARIABLES WHICH IT DETERMINES MAX OF)
EMPIRICAL
LARGEST GROUP CASCADING




ESSENTIAL:

	BOOLOP   IF/ELSE THROUGHOUT, & | ! < > =, 
	INTOP    +,-,*,//,%,LOG2,PROXTOFRIEND/ENEMY/ANY[LOC,LOC]
	LISTOP	 SUM, SIZE, SORT, IN, UNION, INTERSECTION, DISJUNCTION, PERMUTATION, RAND, MAX, MIN, UNITCOUNT, GROUPCOUNT


-- a graph is something like this

-- 1   2
-- |\ /|
-- 3-4-5

-- nodes with connections. 

-- we can represent a graph as a triangular jagged list of booleans. The above, like so


--  [1.2]             -> [0]
--  [1.3 2.3]         -> [1,0]
--  [1.4 2.4 3.4]     -> [1,1,1]
--  [1.5 2.5 3.5 4.5] -> [0,1,0,1]


> rng :: Int -> Int -> [Int]
> rng ornd 1  = [(inst ornd)]
> rng ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : rng nrnd (it-1))



> triangularize :: [Int] -> Int -> [[Int]]
> triangularize []   n = [] 
> triangularize rnds n = do 
>   let (as,bs) = splitAt n rnds
>   as:(triangularize bs (n+1))

> sumll :: [[Int]] -> Int
> sumll [[]]         = 0
> sumll ([]:qs)      = sumll qs
> sumll ((q:qs):qss) = q + sumll (qs:qss)

> randGraph :: Int -> Int -> [[Int]]
> randGraph seed n =  do
>   let rnds = listMod (rng seed (((n-1)*(n-2)) `div` 2)) 2
>   triangularize rnds 1

-- we can tally how many vertices there are by the amount of lists plus one,
-- edges by the total amount of Trues.

-- we could also store it more like a dictionary, like

-- 1 :     3,4
-- 2 :       4,5
-- 3 : 1,    4
-- 4 : 1,2,3,  5 . We can get from a to b quite easily, if we've got our pictures like this. One should note, here, that it
-- 5 :   2,  4     is entirely possible in a graph for a vertex to be connected to itself, but this doesn't affect the EC 
-- at all, because it adds a face and an edge in one fell swoop. So we'll ignore that. If our graph was directed, with arrows 
-- pointing one way or the other instead of ambivalent lines, we might need that other half. We'll ignore that too.

> addZero :: [[Int]] -> [[Int]]
> addZero (gs:[])  = [(0:(reverse gs))]
> addZero (gs:gss) = (0:(reverse gs)):(addZero gss)


 toDict :: [[Int]] -> [[Int]]
 toDict [] = []
 toDict gr = do 
   let v = 1 + len gr
   let e = sumll gr
   toDict2 1 v e gr

 toDict2 :: Int -> Int -> Int -> [[Int]] -> [Int]
 toDict2 i v e gra = if i>v*v then [] else do
   let cv1 = i `div` v
   let cv2 = i `mod` v
   if cv1==cv2 then toDict2 (i+1) v e gra
   else if cv1>cv2 
          then if (gra !! cv1) !! cv2 == 1 
                 then cv2:(toDict2 (i+1) v e gra) 
                 else if (gra !! cv2) !! cv1 == 1 
                        then if i `mod` v == v-1 
                               then cv2:(toDict2 (i+1) v e gra)  
                               else cv2:(toDict2 (i+1) v e gra) 

-- There's also the incidence matrix, which is like
--     e_1 e_2 e_3 e_4 e_5 e_6 
-- v_1   1   1   0   0   0   0
-- v_2   0   0   1   1   0   0
-- v_3   1   0   0   0   1   0
-- v_4   0   1   1   0   1   1
-- v_5   0   0   0   1   0   1

-- And the edge matrix: 

-- [1,3] 
-- [1,4] 
-- [2,4] 
-- [2,5] 
-- [3,4] 
-- [4,5] 


-- a graph's Euler Characteristic is its faces-edges+vertices. we can tell if a graph is planar (can be drawn on a flat 
-- surface with no crossings) if this equals 2. If it doesn't, it can be drawn on a surface with a genus equal to (2-EC)/2.

-- The above graph has 
--   3 faces (1.3.4, 2.4.5., and the infinite abyss)
-- - 6 edges 
-- + 5 vertices
-------
--   2, which we could have figured out by noting that it doesn't cross itself. How can we determine how many faces there 
-- are of a general graph? we could try to figure out all the cycles. 

-- a cycle is a sequence of nodes, between each of which there exists a vertex, of which only the first and last are equal.
-- the two main cycles in our graph are [1,3,4,1] and [2,4,5,2]. We could try to find all of them, then try to get the 
-- minimal amount that are connected to each other and span all vertices.


  1
 234
56789


seq

This section is just the top 50 OEIS sequences in the author's opinion.

A061709 - 1,4,20,104,752

A037271 - 2,1,13,2,4,1,5

A061712 - 2, 3, 7, 23, 31, 311, 127, 383, 991, 2039

A005251 - 0, 1, 1, 1, 2, 4, 7, 12, 21, 37, 65, 114, 200, 351, 616, 1081

A051912 - 0, 1, 4, 13, 32, 71, 124, 218, 375, 572, 744, 1208, 1556

A051917 - 1, 3, 2, 15, 12, 9, 11, 10, 6, 8, 7, 5, 14, 13, 4, 170

A002516 - 0, 3, 6, 2, 12, 7, 4, 10, 24, 11, 14, 18, 8, 15, 20, 26, 48, 19, 22

A000058 - 2, 3, 7, 43, 1807, 3263443, 10650056950807

A003215 - 1, 7, 19, 37, 61, 91, 127, 169, 217, 271

A120498 - 9, 32, 49, 64, 81, 125, 128, 225, 243, 245, 250

A033880 - -1, -1, -2, -1, -4, 0, -6, -1, -5, -2, -10, 4, -12, -4, -6, -1, -16, 3

A007376 - 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 0, 1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 1, 6

A000105 - 1, 1, 1, 2, 5, 12, 35, 108, 369, 1285, 4655, 17073, 63600

A003987 -

0	1	2	3	4	5	6	7	8	9	10	11	12	13
1	0	3	2	5	4	7	6	9	8	11	10	13
2	3	0	1	6	7	4	5	10	11	8	9
3	2	1	0	7	6	5	4	11	10	9
4	5	6	7	0	1	2	3	12	13
5	4	7	6	1	0	3	2	13
6	7	4	5	2	3	0	1
7	6	5	4	3	2	1
8	9	10	11	12	13
9	8	11	10	13
10	11	8	9
11	10	9
12	13
-- 13

A002736 - 	0, 2, 24, 180, 1120, 6300, 33264, 168168, 823680, 3938220

A000326 - 	0, 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, 176, 210

A005188 - 	1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371, 407, 1634, 8208, 9474

A079000 - 	1, 4, 6, 7, 8, 9, 11, 13, 15, 16, 17, 18, 19, 20, 21, 23, 25, 27

A000201 - 1, 3, 4, 6, 8, 9, 11, 12, 14, 16, 17, 19, 21, 22, 24, 25, 27, 29, 30, 32

A001122 - 3, 5, 11, 13, 19, 29, 37, 53, 59, 61, 67, 83, 101, 107, 131, 139

A003226 - 	0, 1, 5, 6, 25, 76, 376, 625, 9376, 90625, 109376, 890625, 2890625

A256111 - 	0, 1, 5, 13, 26, 50, 65, 85, 116, 100, 97, 85

A000110 - 	1, 1, 2, 5, 15, 52, 203, 877, 4140, 21147, 115975

A007318 - 

                                	1	
								1		1	
							1		2		1	
						1		3		3		1	
					1		4		6		4		1	
				1		5		10		10		5		1	
			1		6		15		20		15		6		1	
		1		7		21		35		35		21		7		1	
	1		8		28		56		70		56		28		8		1	
1		9		36		84		126		126		84		36		9		1	

A007623 - 	0, 1, 10, 11, 20, 21, 100, 101, 110, 111, 120, 121, 200, 201

A024630 - 0, 1, 2, 3, 20, 21, 22, 23, 200, 201, 202, 203, 220, 221, 222

A005692 - 	0, 7, 5, 65, 11, 63, 9, 17, 61, 69, 7, 15, 59, 23, 67, 93, 31, 13, 57

A005694 - 	6, 12, 23, 45, 46, 89, 91, 92, 93, 177, 179, 183, 185

A022844 - 	0, 3, 6, 9, 12, 15, 18, 21, 25, 28, 31, 34, 37, 40, 43, 47, 50, 53, 56

A090277 - 	1, 2, 2, 4, 4, 3, 3, 1, 1, 3, 3, 2, 2, 4, 4, 1, 1, 4, 4, 3, 3, 2

A000041 - 	1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490

A000108 - 	1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786

A000213 - 	1, 1, 1, 3, 5, 9, 17, 31, 57, 105, 193, 355, 653, 1201, 2209, 4063

A027641 - 	1, -1, 1, 0, -1, 0, 1, 0, -1, 0, 5, 0, -691, 0, 7, 0

A035250 - 	1, 2, 2, 2, 2, 2, 3, 2, 3, 4, 4, 4, 4, 3, 4, 5, 5, 4, 5

A085104 -   7, 13, 31, 43, 73, 127, 157, 211, 241, 307

A005384 - 	2, 3, 5, 11, 23, 29, 41, 53, 83, 89, 113, 131, 173

A000204 - 	1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843, 1364,

A301849 - 	-1, 0, 1, 0, -1, 1, 1, -1, -1, 0, 1, 1, -1, 0, 1, -1, -1, 0, 1

A006500 -	1, 2, 4, 8, 12, 18, 27, 45, 75, 125, 200, 320, 512, 832, 1352, 2197, 3549, 5733, 9261, 14994, 24276, 39304, 63580

A005208 - 	0, 1, 2, 3, 4, 4, 5, 4, 4, 5, 6, 6, 7, 7, 7, 5, 6, 6, 7, 7, 8, 8, 9, 7, 6, 7, 5, 6, 7, 8, 9, 6, 7, 8, 9, 6, 7, 8, 9, 9, 10, 10, 11, 10

A037306 -   1;
            1,  1;
            1,  1,  1;
            1,  2,  1,  1;
            1,  2,  2,  1,  1;
            1,  3,  4,  3,  1,  1;
            1,  3,  5,  5,  3,  1,  1;
            1,  4,  7, 10,  7,  4,  1,  1;
            1,  4, 10, 14, 14, 10,  4,  1,  1;
            1,  5, 12, 22, 26, 22, 12,  5,  1,  1;
            1,  5, 15, 30, 42, 42, 30, 15,  5,  1,  1;
			
A000007 -	1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

A000001 -	0, 1, 1, 1, 2, 1, 2, 1, 5, 2, 2, 1, 5, 1, 2, 1, 14, 1, 5, 1, 5, 2, 2, 1, 15

A000002 -	1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 1, 2, 2, 1, 2

A00005 - 	1, 2, 2, 3, 2, 4, 2, 4, 3, 4, 2, 6, 2, 4, 4, 5, 2, 6, 2, 6, 4

A000008 -	1, 1, 1, 2, 2, 3, 4, 5, 6, 8, 10, 12, 15, 18, 22, 27, 32, 38, 46, 54

A000005 - 1, 1, 2, 2, 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8, 12, 10, 22, 8, 20, 12, 18

A000014 - 	0, 1, 1, 0, 1, 1, 2, 2, 4, 5, 10, 14, 26, 42, 78, 132, 249, 445, 842, 1561, 2988, 5671, 10981, 21209, 41472, 81181, 160176, 316749, 629933, 1256070, 2515169, 5049816, 10172638

A000032	2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123, 199, 322, 521, 843, 1364

A000123 - 	1, 2, 4, 6, 10, 14, 20, 26, 36, 46, 60, 74, 94, 114, 140, 166, 202, 238

A000217 -	0, 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 1

A000055 - 	1, 1, 1, 1, 2, 3, 6, 11, 23, 47, 106, 235, 551, 1301, 3159, 7741

A000124 - 	1, 2, 4, 7, 11, 16, 22, 29, 37, 46, 56, 67, 79, 92, 106, 121, 137, 154, 172

A000085 - 	1, 1, 2, 4, 10, 26, 76, 232, 764, 2620, 9496, 35696, 140152, 568504

A000088 - 	1, 1, 2, 4, 11, 34, 156, 1044, 12346, 274668, 12005168, 1018997864

A000166 - 	1, 0, 1, 2, 9, 44, 265, 1854, 14833, 133496, 1334961, 14684570

A000396 - 	6, 28, 496, 8128, 33550336, 8589869056, 137438691328

A000129 -	0, 1, 2, 5, 12, 29, 70, 169, 408, 985, 2378, 5741, 13860, 33461, 80782, 195025

A000203 - 	1, 3, 4, 7, 6, 12, 8, 15, 13, 18, 12, 28, 14, 24, 24, 31, 18, 39, 20, 42, 32, 36, 24, 60, 31

SUMMER HASKELL