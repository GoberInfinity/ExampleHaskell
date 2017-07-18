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

