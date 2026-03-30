factorial :: Int -> Int 
factorial n = case n of
              0 -> 1  
              otherwise -> n * factorial (n - 1) 

factorial1 :: Int -> Int 
factorial1 n = if n > 1 then n * factorial1 (n-1) else 1 

factoresPrimos :: Integral a => a -> [a] 
factoresPrimos n = case factors of 
                    [] -> [n]
                    otherwise -> factors ++ factoresPrimos (div n (head factors)) 
                    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2..n-1]

-- factoresPrimos 10 = factors ++ fp ()

fp :: Integral a => a -> [a] 
fp n = [x | x <- [2..n-1], mod n x == 0, esPrimo x]

esPrimo :: Integral a => a -> Bool 
esPrimo n = n >= 2 && null [x | x <- [2..n-1], mod n x == 0]