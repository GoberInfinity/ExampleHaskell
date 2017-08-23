import Data.List.Split.Internals

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

--Time for processing the answer too high
problem_7 = primeP 2 0

primeP :: Int -> Int -> Int
primeP n 10001 =  n - 1
primeP n c = if null [ x | x <- [2..n - 1], n `mod` x  == 0]
                then primeP (n + 1) (c + 1)
                else primeP (n + 1) c



problem_8 = consecutive (tolist $ 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450) []

consecutive [] xs' = maximum $ map (foldl (*) 1 ) $ chunksOf 13 xs'
consecutive list@(x:xs) xs' = consecutive (xs) (xs' ++ (head $ chunksOf 13 list))

tolist n = map (\x -> read [x] :: Int) (show n)
