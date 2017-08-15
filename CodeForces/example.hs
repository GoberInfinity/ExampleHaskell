-- Theatre Square in the capital city of Berland has a rectangular shape with the size n × m meters. On the occasion of the city's anniversary, a decision was taken to pave the Square with square granite flagstones. Each flagstone is of the size a × a.

-- What is the least number of flagstones needed to pave the Square? It's allowed to cover the surface larger than the Theatre Square, but the Square has to be covered. It's not allowed to break the flagstones. The sides of flagstones should be parallel to the sides of the Square.

-- Input
-- The input contains three positive integer numbers in the first line: n,  m and a (1 ≤  n, m, a ≤ 109).

-- Output
-- Write the needed number of flagstones.

-- input
-- 6 6 4
-- output
-- 4

main = do
  [n, m, a] <- fmap (map read . words) $ getLine
  putStrLn . show $ (ceiling (n / a)) * (ceiling (m / a))


-- In the popular spreadsheets systems (for example, in Excel) the following numeration of columns is used. The first column has number A, the second — number B, etc. till column 26 that is marked by Z. Then there are two-letter numbers: column 27 has number AA, 28 — AB, column 52 is marked by AZ. After ZZ there follow three-letter numbers, etc.

-- The rows are marked by integer numbers starting with 1. The cell name is the concatenation of the column and the row numbers. For example, BC23 is the name for the cell that is in column 55, row 23.

-- Sometimes another numeration system is used: RXCY, where X and Y are integer numbers, showing the column and the row numbers respectfully. For instance, R23C55 is the cell from the previous example.

-- Your task is to write a program that reads the given sequence of cell coordinates and produce each item written according to the rules of another numeration system.

-- Input
-- The first line of the input contains integer number n (1 ≤ n ≤ 105), the number of coordinates in the test. Then there follow n lines, each of them contains coordinates. All the coordinates are correct, there are no cells with the column and/or the row numbers larger than 106 .

-- Output
-- Write n lines, each line should contain a cell coordinates in the other numeration system.

-- Examples
-- input
-- 2
-- R23C55
-- BC23
-- output
-- BC23
-- R23C55


import Data.Char
import Data.List

intToStr 0 = []
intToStr x = (intToStr $ (x - 1) `div` 26) ++ [chr $ (x - 1) `mod` 26 + ord 'A']
rowToInt = foldl (\s c -> 26 * s +ord c - ord 'A' + 1) 0

conv xs = case tran xs of
            [_,r,_,c] -> (intToStr . read $ c) ++ r
            [c,r] -> "R" ++ r ++ "C" ++ (show . rowToInt $ c)
  where
    tran = groupBy (\a b -> isDigit a == isDigit b)

main2 = interact $ unlines . map conv . tail . lines



