> canAddTo :: [Int] -> Int -> Bool
> canAddTo xs 0 = True
> canAddTo [] d = False
> canAddTo (x:xs) s = if x > s then canAddTo xs s 
>                              else (canAddTo xs (s-x)) || (canAddTo xs s)


