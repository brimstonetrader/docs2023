COVER SHEET 

3 hours to do

I learned relative sizes of the various asymptotic behaviors in a real-world context. I also learned that n^(1/(k+1)) is o(n^(1/k)), giving me a broader understanding of this scaffolding network of functions.

I should be reviewing 151-era concepts, as I took that in Covid and probably shouldn't count on myself to have learned anything in that epidemiological climate. I have been reading Algorithms in C by Sedgewick to review foundational concepts (I read Ferrer's blog post about C is trash but I've been following this particular set of algorithms well).


MODULE 2


1. Suppose we've a computer that performs 10^10 operations per second. Per hour, it can do 60*60*10^10 = 3.6*10^13 computations. This is the limit of our patience. 

1.1. For an algorithm that scales to its input like Θ(n^2), the largest feasible n-put is (3.6*10^13)^0.5 = 6,000,000

1.2. Θ(n^3): (3.6*10^13)^(1/3) = 33,019

1.3. Θ(100n^2): In this context, the "100" is constant, and theta notation is technically incorrect. Nevertheless, (3.6*10^11)^0.5 = 600,000

1.4. Θ(n*log2(n)): Let 

> log2 :: Integer -> Integer
> log2 1 = 0
> log2 x = 1+(log2 (x // 2))

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b

> findSolution :: Integer -> Integer -> Integer
> findSolution min max = do 
>   let n = (min+max)//2
>   if     (n*(log2 n)) > (36*10^12) then findSolution min n else 
>   	if (n*(log2 n)) < (36*10^12) then findSolution n max else n

ghci> findSolution 0 (36*10^12)
  900000000000
ghci> log2 900000000000
  40
ghci> 40*900000000000
  36000000000000

The largest possible input given our time limits is 900,000,000,000

1.5. Θ(2^n): A special tool that will help us later: suppose a function f(n). This function's inverse, g(n), is such that g(f(n)) = n for every n in f(n)'s domain. The inverse of 2^n is log2(n). Thus, the maximum possible n here is log2(3.6*10^13) = 46

1.6. Θ(2^(2^n)): Exponentiation is right-associative, so those parentheses are unnecessary, but helpful to a casual reader. In this case, the maximum is floor(log2(log2(3.6*10^13))) = 5.

1.7. Θ(n!): 

> fact :: Integer -> Integer
> fact 1 = 1
> fact n = n*(fact (n-1))

ghci> (fact 16) < (36*10^12)
	True
ghci> (fact 17) < (36*10^12)
	False
	
The maximum possible value of n is 16. 

2. The following functions are sorted in order of asymptotic growth rate. 

	  i. (2n)^0.5

	 ii. n+10
	 
	iii. log2(n)*n^2
	
	 iv. n^2.5
	 
	  v. 10^n
	  
	 vi. 100^n
	 
The first function grows by the square root of n, asymptotically slower than the second, which grows linearly. The third grows quadratically, with an additional logarithmic factor. This means that it grows more slowly than the fourth function. To see this, divide both by n^2. log2(n) has 2^n as its inverse, and n^0.5 has n^2 as its inverse. Because we know 2^n > n^2 for sufficiently large n, we can conclude that the reverse is true for each function's inverse. 100^n grows more quickly than 10^n, as can be seen by lim (n -> ∞) (100^n/10^n) = lim (n -> ∞) (10^n).

3. The following questions have the following optimal runtimes, in terms of Θ.

3.1.  1+2+3+4+...+n = (n*(n-1))/2 = 0.5n^2 - 0.5n is Θ(n^2).
	
3.2.  The average number of array lookups to find a given element in a sorted array of length n, by binary search, is Θ(log(n)), as each evaluation cuts the space of possibilities in half.
	
3.3.  The number of two-element subsets of a set of size n is (n choose 2) = (n*(n-1))/2 = (n*(n-1))/2 = 0.5n^2 - 0.5n is Θ(n^2), the same as 3.1.
	
3.4.  1+2+4+8+...+2^n = 2^{n+1}-1 by the homework two weeks ago. Thus, this one is Θ(2^n).

3.5.  The total number of nodes in a complete binary tree with n leaves (or nodes without children) is 2n-1. Thus, the total amount scales to n by a constant factor, and the growth is Θ(1).
	
3.6.  In an n-node graph where every node is connected to every other node, there are n^2 total edges, meaning the asymptotic growth rate of nodes to edges is Θ(n^2).
	
3.7.  To arrange n people in a line, the first may be placed in one of n places, the second in one of n-1 places, and so on down to the nth person having only one possible place. Thus, the growth of possibilities in terms of n is Θ(n!).
	
3.8.  The average number of steps to find an element in a sorted list of length n, considering that each evaluation cuts the space of possibilities in half, is Θ(log(n)).
	
3.9.  The biggest integer that can be represented by n bits is 1+2+4+8+...+2^n-1. By 3.3, this is Θ(2^n).
	
3.10. The worst-case runtime of bubble sort, on a list of size n, is 1+2+3+4+...+n-1. By 3.1, this is Θ(n^2).
	
3.11. Given two unsorted lists of length n, one must be fully read n times for each element of the other, meaning that discerning the two to have no elements in common is Θ(n^2).
	
3.12. for i in range(0,n):
          for j in range (0,i):
              sum += array[i][j], in Python, performs a triangular number of operations, which we've established is Θ(n^2).
				  
3.13. for (int i = 0; i < n; i += 3) {
          sum = sum + i;
      }, in Java, performs n/3 operations, which given that constants don't matter to asymptotic growth is Θ(n)
		  
π.    for (int i = 1; i < n; i *= 2) {
          for (j = 0; j < 20; j++) {
              System.out.println(i + j);
          }
      } performs 20*log2(n) operations, which is Θ(log(n)).
		  
3.15. def foo(n):
          print(n)
          if n > 0:
              foo(n-1)
              foo(n-1) performs 2^n operations, which is Θ(2^n).
				  
4.1.   We have an n-rung ladder, and 2 jars. Instead of going up one rung at a time, dropping at each, and halting once it breaks (linear time), we can use an alternative method which takes square root of n time. Let n=64 arbitrarily. We can go up the ladder n^0.5, or, 8 rungs at a time, meaning that our first sweep takes n^0.5 operations. Once the jar breaks, we know the interval with the breaking point contains n^0.5 rungs. Dropping from each in order takes another n^0.5 operations, meaning in total there are 2*n^0.5 operations. Given that constants do not matter to Big Theta, this process is Θ(n^0.5).
	
4.2.  Suppose now that we have k jars. We can, on our first sweep, drop from every n^(k-1 / k)th rung. This means that our intervals each have size n^(k-1 / k). On our second sweep, we can drop from every (n^(k-1 / k))^(k-2 / k-1)th rung, which evaluates to the same number as before. We can continue this process to get a total number of operations equal to k*n^(1/k), which is Θ(n^(1/k)). As an example consider the cases for k=3 and k=4 on a 4096 rung ladder. 
	
k=3: For the first sweep, drop from rungs 256, 512, 768, ..., 4096. 16 total drops. Let l be the last rung where the jar didn't break. For the second sweep, drop from rungs l+16, l+32, ..., l+256. 16 total drops, with that last one being unnecessary. Let m = the new last rung where the jar didn't break. On the last sweep, the possibility space is 16-long, and we only have one jar left. thus, there are 3*(4096^(1/3)) operations done in total.
		     
k=4: For the first sweep, drop from rungs 512, 1024, 1536, ..., 4096. 8 total drops. Let l be the last rung where the jar didn't break. For the second sweep, drop from rungs l+64, l+128, ..., l+512. 8 total drops, with that last one being unnecessary. Let m = the new last rung where the jar didn't break. For the third sweep, drop from rungs m+8, m+16, ..., m+64. 8 total drops, with that last one being unnecessary. Let o = the new last rung where the jar didn't break. On the last sweep, the possibility space is 8-long, and we only have one jar left. thus, there are 4*(4096^(1/4)) operations done in total.
		     
We can demonstrate that this method is asymptotically faster for k+1 than for k using calculus.
		
lim (n -> ∞) ((k+1) * n^(1 / k+1)) / kn^(1/k)  = 

(k+1 / k) lim (n -> ∞) 1/(n^(1/(k^2 + k))) = 0. 
	      