--Para resolver los ejercicios no está permitido usar recursión explícita, a menos que se indique lo contrario.!!

--Ejercicio 1 ⋆

max2 :: (Float,Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: (Float,Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)

predecesor :: Float -> Float
predecesor = subtract 1

evaluarEnCero :: (Float -> Float) -> Float 
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a)
dosVeces = \f -> f . f

flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]
flipAll = map flip

flipRaro :: (a -> b -> c) -> (a -> b -> c)
flipRaro = flip flip

--Ejercicio 2 

curry :: ((a,b) -> c) -> (a,b) -> (a -> b -> c)
curry = (\f (x,y)-> f x y)

uncurry :: (a -> b -> c) -> a -> b -> ((a, b)-> c)
uncurry = (\f x y -> f (x,y))
