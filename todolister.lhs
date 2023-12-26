> data Month where
>   Jan :: Month
>   Feb :: Month
>   Mar :: Month
>   Apr :: Month
>   May :: Month
>   Jun :: Month
>   Jul :: Month
>   Aug :: Month
>   Sep :: Month
>   Oct :: Month
>   Nov :: Month
>   Dec :: Month
>   deriving (Show, Eq)

> data DayOfWeek where
>   Sun :: DayOfWeek
>   Mon :: DayOfWeek
>   Tue :: DayOfWeek
>   Wed :: DayOfWeek
>   Thu :: DayOfWeek
>   Fri :: DayOfWeek
>   Sat :: DayOfWeek
>   deriving (Show, Eq)

> type Day   = Int
> type Month = Int
> type Date  = (Month, DayOfWeek, Day)

> incDOW :: DayOfWeek -> DayOfWeek 
> incDOW Sun = Mon
> incDOW Mon = Tue
> incDOW Tue = Wed
> incDOW Wed = Thu
> incDOW Thu = Fri
> incDOW Fri = Sat 
> incDOW Sat = Sun

> incDate :: Date -> Date    
> incDate (Jan, w, 31) = (Feb,incDOW w,1) 
> incDate (Feb, w, 28) = (Mar,incDOW w,1) 
> incDate (Mar, w, 31) = (Apr,incDOW w,1) 
> incDate (Apr, w, 30) = (May,incDOW w,1) 
> incDate (May, w, 31) = (Jun,incDOW w,1) 
> incDate (Jun, w, 30) = (Jul,incDOW w,1) 
> incDate (Jul, w, 31) = (Aug,incDOW w,1) 
> incDate (Aug, w, 31) = (Sep,incDOW w,1) 
> incDate (Sep, w, 30) = (Oct,incDOW w,1) 
> incDate (Oct, w, 31) = (Nov,incDOW w,1) 
> incDate (Nov, w, 30) = (Dec,incDOW w,1) 
> incDate (Dec, w, 31) = (Jan,incDOW w,1) 
> incDate (m, w, d)    = (m,incDOW w,d+1) 

> blankCalendar :: Date -> Date -> IO ()
> blankCalendar (m1,w1,d1) d2 = if (m1,w1,d1) == d2 then putStrLn "" else do 
>   (putStrLn ((show w1) ++ ", " ++ (show m1) ++ " " ++ (show d1)))
>   (blankCalendar (incDate (m1,w1,d1)) d2)
 
