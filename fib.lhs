> import Text.Printf
> import Control.Exception
> import System.CPUTime

The fibonacci sequence is like 1 and 1 and so on.

> fibrec :: Integer -> Integer
> fibrec 0 = 0
> fibrec 1 = 1
> fibrec n = fibrec (n-1) + fibrec (n-2)

		ghci> fibrec 33
		3524578
		-- took more than 15 seconds to run.

What if we want the 75th term? Dr. Yorgey is right: floats can’t help us, not even with the Lucas numbers. Infinite fractions, while easily defined recursively and super cool, are for later.

enter matrices of format

| a b |  
| b c |.  "symmetrics".

| a b |   | d e |   | ad+be bd+ce |
| b c | # | e f | = | bd+ce be+cf |, 

which preserves symmetry, as can be demonstrated by bd+ce equals itself.

Observe the behavior of the a=b=1, c=0 matrix on repeated multiplications:

		ghci> [[1,1],[1,0]] # [[1,1],[1,0]]

		  [[2,1],[1,1]]

		ghci> [[2,1],[1,1]] # [[1,1],[1,0]]
  
		  [[3,2],[2,1]]

		ghci> [[3,2],[2,1]] # [[1,1],[1,0]]

		  [[5,3],[3,2]]

		ghci> [[5,3],[3,2]] # [[1,1],[1,0]]

		  [[8,5],[5,3]]

I will note ominously that matrix multiplication is associative, appealing to Wikipedia.

Hey, let’s say we have a symmetric matrix where a=b+c. One such matrix is a=b=1,c=0, but it could be any.

		| a b |   | 1 1 |   | a+b a |   | a+b a |
		| b c | # | 1 0 | = | b+c b | = |  a  b |. 

We know that the symmetry will remain, and a=b+c. The property of d=e+f also remains, with each new term being the sum of the other two. These properties remain on repeated iterations, we can conclude inductively, and a sequence of sums of the two previous terms, starting with two ones, must be the Fibonacci. Thus,

		| 1 1 |        | F_n+1  F_n  |
		| 1 0 | ^ n =  | F_n    F_n-1|
		 
		 
		ghci> mpow [[1,1],[1,0]] 2
		[[2,1],[1,1]]
		ghci> mpow [[1,1],[1,0]] 3
		[[3,2],[2,1]]
		ghci> mpow [[1,1],[1,0]] 4
		[[5,3],[3,2]]
		ghci> mpow [[1,1],[1,0]] 5
		[[8,5],[5,3]]

		...

		ghci> mpow [[1,1],[1,0]] 75
		[[3416454622906707,2111485077978050],[2111485077978050,1304969544928657]]

Because matrix multiplication is associative, we can format our brackets to look something like a binary tree without changing the answer. For example, if we square [[1,1],[1,0]] 6 times, the diagonal will read the 64th fibonacci number. This can be exploited to only do log_2 n matrix multiplications, giving us fibonacci numbers even faster.

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

> fib :: Integer -> Integer 
> fib i = (binarympow [[1,1],[1,0]] i) !! 1 !! 0

		ghci> fib 33
		3524578
		-- same answer as recursive, in microseconds.

		ghci> fib 75
		2111485077978050
		ghci> fib 74
		1304969544928657
		ghci> fib 73
		 806515533049393

		 QUICK!

		ghci> fib 14
		377
		ghci> fib 140
		81055900096023504197206408605
		ghci> fib 1400
		17108476902340227241249719513231821477382749898026920041550883749834348017250935801359315038923367841494936038231522506358371361016671790887791259870264957823133253627917432203111969704623229384763490617075388642696139893354058660570399927047816296952516330636633851111646387885472698683607925
		ghci> fib 14000
		3002468761178461090995494179715025648692747937490792943468375429502230242942284835863402333575216217865811638730389352239181342307756720414619391217798542575996541081060501905302157019002614964717310808809478675602711440361241500732699145834377856326394037071666274321657305320804055307021019793251762830816701587386994888032362232198219843549865275880699612359275125243457132496772854886508703396643365042454333009802006384286859581649296390803003232654898464561589234445139863242606285711591746222880807391057211912655818499798720987302540712067959840802106849776547522247429904618357394771725653253559346195282601285019169360207355179223814857106405285007997547692546378757062999581657867188420995770650565521377874333085963123444258953052751461206977615079511435862879678439081175536265576977106865074099512897235100538241196445815568291377846656352979228098911566675956525644182645608178603837172227838896725425605719942300037650526231486881066037397866942013838296769284745527778439272995067231492069369130289154753132313883294398593507873555667211005422003204156154859031529462152953119957597195735953686798871131148255050140450845034240095305094449911578598539658855704158240221809528010179414493499583473568873253067921639513996596738275817909624857593693291980841303291145613566466575233283651420134915764961372875933822262953420444548349180436583183291944875599477240814774580187144637965487250578134990402443365677985388481961492444981994523034245619781853365476552719460960795929666883665704293897310201276011658074359194189359660792496027472226428571547971602259808697441435358578480589837766911684200275636889192254762678512597000452676191374475932796663842865744658264924913771676415404179920096074751516422872997665425047457428327276230059296132722787915300105002019006293320082955378715908263653377755031155794063450515731009402407584683132870206376994025920790298591144213659942668622062191441346200098342943955169522532574271644954360217472458521489671859465232568419404182043966092211744372699797375966048010775453444600153524772238401414789562651410289808994960533132759532092895779406940925252906166612153699850759933762897947175972147868784008320247586210378556711332739463277940255289047962323306946068381887446046387745247925675240182981190836264964640612069909458682443392729946084099312047752966806439331403663934969942958022237945205992581178803606156982034385347182766573351768749665172549908638337611953199808161937885366709285043276595726484068138091188914698151703122773726725261370542355162118164302728812259192476428938730724109825922331973256105091200551566581350508061922762910078528219869913214146575557249199263634241165352226570749618907050553115468306669184485910269806225894530809823102279231750061652042560772530576713148647858705369649642907780603247428680176236527220826640665659902650188140474762163503557640566711903907798932853656216227739411210513756695569391593763704981001125

fib of one million was much longer than I feel like including but calculated in under ten seconds. The last digit is five. Note

		ghci> 140 % 60
		20
		ghci> 1400 % 60
		20
		ghci> 14000 % 60
		20
		ghci> 1000000 % 60
		40

		ghci> fib 20
		6765
		ghci> fib 40
		102334155
		
Then there's a tail recursive version.

> fibtail :: Integer -> Integer
> fibtail n = f n (0, 1)
>          
> f :: Integer -> (Integer, Integer) -> Integer 
> f 0 (a,b) = a
> f n (a,b) = f (n - 1) (b, a + b)
	
> 	
	
	
from https://stackoverflow.com/questions/3980756/is-this-fibonacci-sequence-function-recursive
	
> fibfinity :: [Integer]	
> fibfinity=0:1:zipWith (+) fibfinity (tail fibfinity)






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
>     putStrLn "Starting..."
>     time $ fibtail 100000 `seq` return ()
>     putStrLn "Done."

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

