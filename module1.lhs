Look and Say Sequence

The Look and Say Sequence, introduced by John Conway, begins as
follows:

1, 11, 21, 1211, 111221, 312211, . . .

where each sequence of digits “describes” the previous sequence;
read the Wikipedia article for a full description

Write a Haskell function getRun :: [Integer] -> ([Integer], [Integer])
which splits its input list into two pieces, a run of consecutive equal
digits at the beginning, and the rest. For example,

getRun [1,1,1,2,3] = ([1,1,1], [2,3])
getRun [3,2,1] = ([3], [2,1])
getRun [1] = ([1],[])

If you find it helpful, you are welcome to write additional helper
function(s).

> getRun  :: [Integer] -> ([Integer], [Integer])
> getRun list = getRun2 list ([],[])

> getRun2 :: [Integer] -> ([Integer], [Integer]) -> ([Integer], [Integer])
> getRun2        []  it       = it
> getRun2    (a:as) ([],[])   = getRun2 as ([a],[])
> getRun2    (a:as) (x:xs,[]) = if a==x then getRun2 as (a:x:xs,[]) else (x:xs,a:as)


Now write a Haskell function lookAndSay :: [Integer] -> [Integer]
which outputs the next digit sequence when given a digit sequence
as input. For example

  lookAndSay [1] = [1,1]
  lookAndSay [1,1,1,2,2,1] = [3,1,2,2,1,1]

Of course, you should use your getRun function from the previous
exercise.

> lookAndSay :: [Integer] -> [Integer]
> lookAndSay []  = []
> lookAndSay l = do
>   let (b:bs, c) = getRun l
>   let a = fromIntegral (length (b:bs))
>   a:b:(lookAndSay c)


Finally, write a function lookAndSaySeq :: Integer -> [[Integer]],
where lookAndSaySeq n produces the first n terms of the look and
say sequence. For example,
lookAndSaySeq 4 = [[1], [1,1], [2,1], [1,2,1,1]]

> lookAndSaySeq :: Integer -> [[Integer]]
> lookAndSaySeq n = init (lookAndSaySeq2 [1] 0 n)

> lookAndSaySeq2 :: [Integer] -> Integer -> Integer -> [[Integer]]
> lookAndSaySeq2 w x y = if x==y then [[]] else do
>   let z = (lookAndSay w)
>   w:(lookAndSaySeq2 z (x+1) y)

    The Towers of Hanoi is a classic puzzle with a solution
    that can be described recursively. Disks of different sizes are stacked
    on three pegs; the goal is to get from a starting configuration with
    all disks stacked on the first peg to an ending configuration with all
    disks stacked on the last peg.

    The only rules are

      • you may only move one disk at a time, and
      • a larger disk may never be stacked on top of a smaller one.

    For example, as the first move all you can do is move the topmost,
    smallest disk onto a different peg, since only one disk may be moved
    at a time.

> type Peg = String
> type Move = (Peg, Peg)

    Write a function of type Integer -> Peg -> Peg -> Peg -> [Move].

    Given the number of discs and names for the three pegs, hanoi
    should return a list of moves to be performed to move the stack of
    discs from the first peg to the last.
    Note that a type declaration, like type Peg = String above, makes
    a type synonym. In this case Peg is declared as a synonym for String,
    and the two names Peg and String can now be used interchangeably.
    Giving more descriptive names to types in this way can be used to
    give shorter names to complicated types, or (as here) simply to help
    with documentation.
    Example: hanoi 2 "a" "b" "c" == [("a","b"), ("a","c"), ("b","c")]



Let's note (and draw) some small cases

1 disc:                             (ac), 2^1 - 1
                         
2 disc:                         (ab, ac, bc), 2^2 - 1
                            
3 disc:                 (ac, ab, cb, ac, ba, bc, ac), 2^3 - 1
                                               
4 disc: (ab, ac, bc, ab, ca, cb, ab, ac, bc, ba, ca, bc, ab, ac, bc), 2^4 - 1 





                            1     1
                            ___ ___
                            0     1
                                 ac

                        1             1
                        2   21   12   2     
                        ___ ___ ___ ___
                        0   1   2   3
                            ab  ac  bc

                1                             1 
                2   2        1   1        2   2    
                3   3 1 321 32   23 123 1 3   3                            
                ___ ___ ___ ___ ___ ___ ___ ___ 
                0   1   2   3   4   5   6   7   
                    ac  ab  cb  ac  ba  bc  ac  

1                                                             1 
2   2                        1   1                        2   2   
3   3   3   3 1   1 1   12   2   2   21   1 1   1 3   3   3   3                    
4   41  412 4 2 432 432 43  43   34  34 234 234 2 4 214  14   4                                             
___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
    ab  ac  bc  ab  ca  cb  ab  ac  bc  ba  ca  bc  ab  ac  bc

The obvious note is the symmetry exhibited by our ideal solutions. With this in mind, 
we can declare that of our six possible moves, the following act as "inverses", due to 
their reflections match up like a mirror.

cb <-> ba
ab <-> bc

ac <-> ac
ca <-> ca

Let's look at just the first part of each move.

             a a
             aab aab
             aacabba aacabba
             aabaccaabbcbaab

So you just repeat the last one, replacing bs for cs and vice versa, then say "a", then repeat
it again, with a->b, b->c, and c->a. 

   
We can note that there is always an "ac" cute in the middle. Let's put the whole of 3 in a table.


_____                                                       _____
1|a|c                                                       1|a|c
2|a|b                                                       2|a|c
3|c|b                                                       3|c|a
4|a|c   that's cool but you should flip that last column.   4|a|c   
5|b|a                                                       5|b|b
6|b|c                                                       6|b|b
7|a|c                                                       7|a|c

We can generate the other half of our tuples like this! a<->c, b constant, and reverse.


> hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
> hanoi 1 a b c =       [(a,c)]
> hanoi n a b c = (switch c b [] (hanoi (n-1) a b c)) ++ ((a,c):(switch a b [] (hanoi (n-1) a b c)))

> switch :: Peg -> Peg -> [Move] -> [Move] -> [Move]
> switch _ _ ds []         = reverse ds
> switch a b ds ((x,y):ms) = switch b a (((if x==a then b else (if x==b then a else x)),(if y==a then b else (if y==b then a else y))):ds) ms



Exercise 7 (Level 2) What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “tempo-
rary” storage instead of only one. Write a function similar to hanoi
which solves this problem.

It should be possible to do it in far fewer moves than with three
pegs. For example, with three pegs it takes 2^15-1 = 32767 moves
to transfer 15 discs. With four pegs it can be done in only 129 moves!
However, you need not worry about making your function as efficient
as possible, as long as it does significantly better than with three
pegs. See Exercise 1.17 in Graham, Knuth,
and Patashnik, Concrete Mathematics,
second ed., Addison-Wesley, 1994.
If you are stuck, feel free to search for more information on the
Internet; be sure to cite any sources you use.


                    1       1
                    ____ ____
                         ad
               
               1                 1 
               2    21    1 2    2    
               ____ ____ ____ ____ 
                    ac   ad   cd 
          1                           1 
          2    2                 2    2    
          3    31   312   123  1 3    3         
          ____ ____ ____ ____ ____ ____ 
               ab   ac   ad   cd   bd
1                                               1
2    2                                     2    2
3    3    3           2    2          3    3    3
4    41   41 2 4132 413   134 2134 21 4  1 4    4      
____ ____ ____ ____ ____ ____ ____ ____ ____ ____
     ab   ad   ac   dc   ad   ca   cd   ad   bd


d <-> c
a <-> c


1,3,7,15,31,63,127
1,3,5,9 ,17,33,65 (a mild improvement)


> hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
> hanoi4 2 a b c d = [(a,b),(a,d),(c,d)]
> hanoi4 n a b c d = do
>   let xs   = init (switch d c [] (hanoi4 (n-1) a b c d))
>   let y:ys = (switch a c [] (hanoi4 (n-1) a b c d))
>   xs ++ ((a,d):ys)
