 :l OneDrive/Documents/generatingfunctions.lhs
The unifying thread that intrigues me with continued fractions and generating functions is the prospect of finite computations capable of infinite precision to one's precise needs on iteration. The most simplistic application of a generating function that I can think of is the computation of n choose k. The textbook definition is 

> choose :: Integer -> Integer -> Integer
> choose n k = (factorial n) // ((factorial k)*(factorial (k-n)))

ghci> choose 5 2
  60
ghci> choose 7 3
  840
ghci> choose 100 50
  3068518756254966037202730459529469739228459721684688959447786986982158958772355072000000000000

These numbers make an interesting picture, "Pascal's Triangle", even though below it's a square.

   /this top one is n_0_0. 
  /       /this one is n_1_3.
1<1 1  1 /1   1
1 2 3  4< 5   6
1 3 6  10 15  21
1 4 10 20 35  56
1 5 15 35 70  126
1 6 21 56 126 252

n_a_b = n_a-1_b + n_a_b-1. Properties like this are called "recurrance relations". Sometimes the numbers that come out are being guided by normal arithmetic rules such as in this case. In fact, any polynomial of degree k can be written as a recurrance relation with at most k+2 terms ( https://pure.mpg.de/rest/items/item_3150398_3/component/file_3279123/content ).

> chooseRec :: Integer -> Integer -> Integer
> chooseRec n k = if n<=1 || k<=1 then 1 
>                   else (chooseRec (n-1) k) + (chooseRec n (k-1))

This version takes much longer to compute, doing an exponentially growing amount of operations. chooseRec 100 50 takes a long time is the proof. There is a third method, a generating function. To do one of these, we take a product like 
  
n+k
 Π  (x+1)^i   , and then find the coefficient of the x^n. 
i=0

> type Polynomial = [Int]


> dot :: [Int] -> [Int] -> [Int]
> dot [] [] = []
> dot (x:xs) (y:ys) = (x*y):(dot xs ys)


> factorial :: Integer -> Integer
> factorial n|n<2 = 1
> factorial n = n*(factorial (n-1))

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b

CONVOLUTIONS

-- the basic way to multiply polynomials is to convolute them as lists, which is cool

> convolve :: (Num a) => [a] -> [a] -> [a]
> convolve xs ys = do 
>   let dgre = (len xs) + (len ys) - 1
>   let (js,ks) = ((replicate (dgre - (len xs)) 0) ++ xs,(replicate (dgre - (len ys)) 0) ++ ys)
>   map (sum . zipWith (*) (reverse js)) (tails ks)

> convolve = polynomialMultiplication

- map takes in a f(x) and a [x], and applies the function to every x in the list. 
- zipwith takes in a function which takes two arguments, and two lists, outputting 
  a single list which applies the operation to both. for example, 
  zipWith (+) [1,2] [3,4] = [4,6].

ghci> convolve [1,2,1] [1,2,1]
  [1,4,6,4,1]
ghci> convolve [1,2,1] [1,3,3,1]
  [1,5,10,10,5,1]
ghci> convolve [1,4,6,4,1] [1,3,3,1]
  [1,7,21,35,35,21,7,1]


We can use this to calculate (x choose y) with generating functions. 

1 / 1-x    = [1,1,1 ...]
1/ (1-x)^2 = [1,0,1,0,1,0 ...]
1/ (1-x)^3 = [1,0,0,1,0,0,1,0,0 ...]

We don't need an infinite list though, just as long as necessary. This is useful 
because the generating function for partitions of m is 

 m
 _    1
| | -----
i=1 1-x^i

 

  	  

> tails :: [a] -> [[a]]
> tails [] = []
> tails (x:xs) = (x:xs):(tails xs)


> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

				 
				 
DIVIDE AND CONQUER 
