multiply = sum $ [x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0]

sumFibonacci = fibonacciAux 4000 0 1 0

fibonacciAux 0 a2 a1 s = s
fibonacciAux n a2 a1 s = let sumF = a2 + a1
                             evenP = even sumF
                         in if a1 < 4000000
                            then
                              if evenP
                              then
                                fibonacciAux (n - 1) a1 (a2 + a1) (s+sumF)
                              else
                                fibonacciAux (n - 1) a1 (a2 + a1) s
                           else s

primeFactors n = factor n $ [2] ++ [3,5..]
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

problem_3 = last (primeFactors 600851475143)

problem_4 =
  maximum [x | y<-[100..999], z<-[y..999], let x=y*z, let s=show x, s==reverse s]

problem_5 = foldr1 lcm [1..20]

problem_6 = (sum [1..100])^2 - sum (map (^2) [1..100])

problem_7 = primeP 2 0 

primeP :: Int -> Int -> Int
primeP n 10001 =  n - 1
primeP n c = if null [ x | x <- [2..n - 1], n `mod`x  == 0]
                then primeP (n + 1) (c + 1) 
                else primeP (n + 1) c



