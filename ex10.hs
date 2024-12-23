-- write function elem that determines whether a given element is a member of a list
elem' :: Eq a => a -> [a] -> Bool --the type of elem
elem' _ [] = False
elem' x (y:ys) 
    | x == y    = True
    | otherwise = elem' x ys

-- can you use fold?
elem_f :: Eq a => a -> [a] -> Bool
elem_f e l = foldr (\x acc -> (e == x) || acc) False l

-- rewrite partition using fold
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition pd ls = foldr myfunc ([],[]) ls
  where
    myfunc x (first , second)
        | pd x      = (x : first, second)
        | otherwise = (first, x : second)

-- define foldl in terms of foldr
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' func acc ls = foldr myfunc acc ls
  where
    myfunc x a = func a x
-- foldl' (-) 0 [1, 2, 3, 4]    Result : -10
-- foldl' (+) 0 [1, 2, 3]       Result : 6


-- define type Month whose values are months in a year
data Month = January | February | March | April | May | June | July | August | September | October | November | December
    deriving (Show)

--implement function daysInMonth
daysInMonth :: Month -> Integer
daysInMonth m = case m of
    January   -> 31
    February  -> 28 
    March     -> 31
    April     -> 30
    May       -> 31
    June      -> 30
    July      -> 31
    August    -> 31
    September -> 30
    October   -> 31
    November  -> 30
    December  -> 31

nextMonth :: Month -> Month
nextMonth m = case m of
    January   -> February
    February  -> March
    March     -> April
    April     -> May
    May       -> June
    June      -> July
    July      -> August
    August    -> September
    September -> October
    October   -> November
    November  -> December
    December  -> January


-- implement function nextDay
nextDay :: Integer -> Month -> (Integer, Month)
nextDay d m 
    | d == daysInMonth m  = (1,nextMonth m)
    | otherwise           = (d+1,m)