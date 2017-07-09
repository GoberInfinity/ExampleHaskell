-- 15 Replicate the elements of a list a given number of times.
-- (repli '(a b c) 3) => (A A A B B B C C C)
repli :: [a] -> Int -> [a]
repli xs n = concatMap (take n . repeat) xs

-- 16 Drop every N'th element from a list.
-- dropEvery "abcdefghik" 3 => "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs 1
  where helper :: [a] -> Int -> [a]
        helper [] _ = []
        helper (x:xs) i
          | i == n  = helper xs 1
          | i /= n  = x:helper xs (i + 1)

-- 17 Split a list into two parts; the length of the first part is given
-- (split '(a b c d e f g h i k) 3) => ("abc", "defghik")
split = flip splitAt

--Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
--slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 => "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b - a + 1) $ drop (a - 1) xs
