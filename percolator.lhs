> import qualified Data.Map.Strict as M
> import qualified Data.Maybe (fromMaybe)

> type UnionFind a = M.Map a a 
> type HeightMap a = M.Map a Int

> new :: (Ord a) => [a] -> (UnionFind a, HeightMap a)
> new xs = (M.fromList [(x,x) | x <- xs], M.fromList [(x,1) | x <- xs])

> find :: (Ord a) => a -> UnionFind a -> a
> find x map = case M.lookup x map of 
>   Just y  -> if y==x then x else find y map
>   Nothing -> x

> union :: (Ord a) => a -> a -> (UnionFind a, HeightMap a) -> (UnionFind a, HeightMap a)
> union a b (uf, hm) = case (M.lookup a hm, M.lookup b hm) of
>   (Just ah, Just bh) -> do 
>     let (ar, br) = (find a uf, find b uf)
>     if ah < bh
>       then (M.insert br ar uf, M.insert a (bh + ah) hm)
>       else if ah > bh then (M.insert ar br uf, M.insert a (bh + ah) hm) else (uf,hm)
>   _ -> (uf, hm)


> connected :: (Ord a, Eq a) => a -> a -> UnionFind a -> Bool
> connected a b uf = find a uf == find b uf



----------------------------------------------------------------
--
-- Percolation
--

-- Random Number Generator
--
-- Standard
--
--  Chapter 7.1, Eq. 7.1.6
--  parameters from Knuth and H. W. Lewis

> inst :: Int -> Int
> inst r = (16807*r) `mod` ((2 ^ 31)-1) 

> instPercolationChallenge :: Int -> Int -> IO ()
> instPercolationChallenge n rngSeed = do 
>   let grid = rep 1 (n*n)
>   let (uf,hm) = new [(-2)..n*n-1]
>   let a = unionsides [0..n-1] [n*(n-1)..n*n-1] uf hm
>   percolator n grid (rngSeed % n) (inst (rngSeed+1)) a

> unionsides :: [Int] -> [Int] -> UnionFind Int -> HeightMap Int -> (UnionFind Int, HeightMap Int)
> unionsides []      []      uf hm = (uf,hm)
> unionsides (t:top) (b:btm) uf hm = do 
>   let (uf2,hm2) = union t (-1) (uf,hm) 
>   let (uf3,hm3) = union b (-2) (uf2,hm2) 
>   unionsides top btm uf3 hm3 

> percolator :: Int -> [Int] -> Int -> Int -> (UnionFind Int, HeightMap Int) -> IO ()
> percolator n grid pos rnd (uf, hm) = do
>   let ns = findNeighbors pos n grid
>   let (fh, s : sh) = splitAt pos grid
>   let newgrid = (fh ++ (0:sh))
>   let (uf2, hm2) = changeMaps uf hm pos ns
>   if connected (-1) (-2) uf2
>     then print (putToGUI (rect n newgrid))
>     else do
>       let bs = blocked 0 grid
>       let b = bs !! (rnd `mod` (length bs))
>       if not (null bs) then
>         percolator n newgrid b (inst rnd) (uf2, hm2)
>       else
>         print "No path found."




> putToGUI :: [[Int]] -> String
> putToGUI [] = ""
> putToGUI ([]:gss) = "\n" ++ (putToGUI gss)
> putToGUI ((0:gs):gss) = 'X':(putToGUI (gs:gss))
> putToGUI ((1:gs):gss) = ' ':(putToGUI (gs:gss))

> changeMaps :: UnionFind Int -> HeightMap Int -> Int -> [Int] -> (UnionFind Int, HeightMap Int)
> changeMaps uf hm _ [] = (uf, hm)  
> changeMaps uf hm b (n:ns) = union n b (uf,hm)

> blocked :: Int -> [Int] -> [Int]
> blocked _ []     = []
> blocked i (c:cs) = if c==0 then blocked (i+1) cs else i:(blocked (i+1) cs)

> findNeighbors :: Int -> Int -> [Int] -> [Int]
> findNeighbors pos n grid = do
>   let top = if pos > n - 1     && (grid !! (pos - n)) == 0 then [pos - n] else []
>   let lft = if pos % n > 0     && (grid !! (pos - 1)) == 0 then [pos - 1] else []
>   let rit = if pos % n < n - 1 && (grid !! (pos + 1)) == 0 then [pos + 1] else []
>   let btm = if pos + n < n * n && (grid !! (pos + n)) == 0 then [pos + n] else []
>   top ++ lft ++ rit ++ btm
  


> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b


> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: a -> Int -> [a]
> rep a 0 = []
> rep a x = a:(rep a (x-1))

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y

> rect :: Int -> [a] -> [[a]]
> rect n []   = []
> rect n xss  = do 
>   let (yss,zss) = splitAt (fromIntegral n) xss
>   yss:(rect n zss)

[[0,1,0,0,1],
 [1,0,1,0,0],
 [0,1,0,0,0],
 [0,1,0,1,0],
 [1,1,0,1,0]]