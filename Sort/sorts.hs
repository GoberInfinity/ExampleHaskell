import Debug.Trace

sort :: (Ord a,Show a) => [a] -> [a]
sort [] = []
sort (x : xs) = trace ("x = " ++ show x ++ " xs = " ++ show xs) $ insert x (sort xs)

insert :: (Ord a,Show a) => a -> [a] -> [a]
insert x [] =  trace ("x1 = " ++ show x) $ x : []
insert x (y : ys) = trace ("x1 = " ++ show x ++
                           " y1 = " ++ show y ++
                           " ys1 = " ++ show ys)
                    $ insert' (compare x y) x y ys

insert' :: (Ord a,Show a) => Ordering -> a -> a -> [a] -> [a]
insert' LT  x y ys =  trace ("x2 = " ++ show x ++
                           " y2 = " ++ show y ++
                           " ys2 = " ++ show ys) $ x : (y : ys)
insert' _   x y ys = trace ("x2 = " ++ show x ++
                           " y2 = " ++ show y ++
                           " ys2 = " ++ show ys) $ y : insert x ys

worstCase = [5,4,3,2,1]
bestCase = [1,2,3,4,5]


