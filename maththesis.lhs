> isItRotationallySymmetric :: String -> Bool
> isItRotationallySymmetric bitstring =
>     let gnirtstib = reverse bitstring
>     in gnirtstib == bitstring

> toBinary :: Int -> String
> toBinary 0 = "0"
> toBinary 1 = "1"
> toBinary x = toBinary (x // 2) ++ show (x % 2)

> generateBitStrings :: Int -> [String]
> generateBitStrings n = [padZeros n (toBinary i) | i <- [0..2^n-1]]
>     where
>         padZeros n str
>             | length str < n = replicate (n - length str) '0' ++ str
>             | otherwise = str

> toBools :: Int -> [Bool]
> toBools 0 = [False]
> toBools 1 = [True]
> toBools x = (toBools (x // 2)) ++ (toBools (x % 2))


> framesToN :: Int -> [[Bool]]
> framesToN n = [(replicate (2 * n - 1 - (len (toBools x))) False) ++ (toBools x) | x <- [0..2^(2*n-1)-1]]














 noSumToOne :: Int -> [Int]
 noSumToOne n = [x | x <- generateBitStrings n, 
                  (any (== 1) [sum [charToBit (grid !! (3 * j)), 
                                    charToBit (grid !! (3 * j + 1)), 
                                    charToBit (grid !! (3 * j + 2)), 
                                    charToBit (grid !! ((3 * j + 3) `mod` 12))] | j <- [0..3]])]

 charToBit :: Char -> Int	
 charToBit '1' = 1	
 charToBit  _  = 0	












for i in range(len(figaro)):
	grid = figaro[i]
	for j in range(4):
		if (int(grid[(3 * j) + 0]) + int(grid[(3 * j) + 1]) + int(grid[(3 * j) + 2]) + int(grid[((3 * j) + 3) % 12]) == 1):
		figaro[i] = "blunt"
treebytree = []
for i in range(4096):
if figaro[i] != "blunt":
treebytree.append(figaro[i])
print("There are " + str(len(treebytree)) + " elements of this list
right now.")	
	
	
 noSumToOne :: Int -> [Int]
 noSumToOne n = do 
     let grid = generateBitStrings 12
     let set  = [1,2,3,4,4,5,6,7,7,8,9,10,10,11,12,1]
     [sum [read [grid !! (set !! (4*j))] | j <- [0..3], k <- [0..3] ]]

 main :: IO ()
 main = do
     let figaro = generateBitStrings 12
     
     -- Process the list to replace some elements with "blunt"
     let figaro' = [if isBlunt grid 12 then "blunt" else grid | grid <- figaro]
     
     -- Filter out "blunt" elements
     let treebytree = filter (/= "blunt") figaro'
     
     putStrLn $ "There are " ++ show (length treebytree) ++ " elements of this list right now."

















> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)