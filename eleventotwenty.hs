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
