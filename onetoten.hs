--1 Find the last element of a list.
--  myLast [1,2,3,4] => 4
myLast :: [a] -> Maybe a

myLast []     = Nothing
myLast [x]    = Just x
myLast (_:xs) = myLast xs

--2 Find the last but one element of a list.
-- myButLast [1,2,3,4] => 3
myButLast :: [a] -> Maybe a

myButLast []     = Nothing
myButLast [x,_]  = Just x
myButLast (_:xs) = myButLast xs

--3 Find the K'th element of a list. The first element in the list is number 1.
-- elementAt [1,2,3] 2 => 2
elementAt :: [a] -> Int -> Maybe a

elementAt [] _     = Nothing
elementAt (x:_)  1 = Just x
elementAt (_:xs) i = elementAt xs (i-1)

--4 Find the number of elements of a list.
-- myLength [123, 456, 789] => 3
myLength :: [a] -> Int

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

--5 Reverse a list.
-- myReverse [1,2,3,4] => [4,3,2,1]
myReverse :: [a] -> [a]

myReverse []  = []
myReverse (x:xs) = myReverse xs ++ [x]

--6 Find out whether a list is a palindrome.
-- isPalindrome [1,2,4,8,16,8,4,2,1] => True
isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome []   = True
isPalindrome [_]  = True
isPalindrome xs   = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

--8 Eliminate consecuive duplicates of list elements.
-- compress "aaaabccaadeeee" => "abcade"
compress :: (Eq a) => [a] -> [a]

compress [x]    = [x]
compress (x:xs) = if (head xs) == x
                  then compress xs
                  else [x] ++ compress xs

--9  Pack consecutive duplicates of list elements into sublists. If a list contains
--    repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
--      => ["aaaa","b","cc","aa","d","eeee"]
pack :: (Eq a) => [a] -> [[a]]

pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x == (head xs)
              then (x:(head $ pack xs)):(tail $ pack xs)
              else [x]:(pack xs)
