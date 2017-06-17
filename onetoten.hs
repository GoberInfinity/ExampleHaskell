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
elementAt :: [a] -> Maybe a

