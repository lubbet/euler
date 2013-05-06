
primes n = primes2 [] [2,3..n]

-- a - primes found so far
-- p - still sieving
primes2 a [] = a
primes2 a p = 
  primes2 a' p'
  where
    next = head p
    a' = next:a
    p' = filter (\n -> n `mod` next /= 0) p

factor n = filter (\x -> n `mod` x == 0) (primes (n `div` 2))

main = print (take 1 (factor 600851475143))