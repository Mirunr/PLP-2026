--Para resolver los ejercicios no está permitido usar recursión explícita, a menos que se indique lo contrario.!!

--Ejercicio 1 ⋆

max2 :: (Float,Float) -> Float
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: (Float,Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract1 :: Float ->  Float -> Float
subtract1 = flip (-)

predecesor :: Float -> Float
predecesor = subtract1 1

evaluarEnCero :: (Float -> Float) -> Float 
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a)
dosVeces = \f -> f . f

flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

--Ejercicio 2 

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry = (\f (x,y)-> f x y)

curry :: ((a, b) -> c) -> a -> b -> c
curry = (\f x y -> f (x,y)) 

-- Ejercicio 3

{--Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y devuelva su versión currificada?

rta: no, porq la signatura de la función debería adaptarse al número de argumentos según vaya cambiando la cantidad de argumentos.
Es decir no existe una tupla de tamaño indefinido que pueda usarse como comodín en la signatura para ir recorriendo y deshaciéndola como 
--por ejemplo sería una lista...
--}

{--
Considerar las siguientes definiciones:--}
listaDesde x = x : (listaDesde (x+1))

esMultiploDe10 n = mod n 10 == 0

takeHastaMultiploDe10 [] = []
takeHastaMultiploDe10 (x:xs) = if esMultiploDe10 x then [] else x : (takeHastaMultiploDe10 xs)

{--Mostrar paso a paso cómo reduce Haskell la expresión takeHastaMultiploDe10 (listaDesde 29)

takeHastaMUltiploDe10 (listaDesde 29) 
-> takeHastaMultiploDe10 (29:listaDesde(29+1)) 
-> 29 : takeHastaMultiplode10 (listaDesde30)
-> 29 : takeHastaMultiploDe10 (30:listaDesde(30+1))
-> 29 : [] -> [29]

--}

--Ejercicio 4

paresDeNat :: [(Int,Int)]
paresDeNat = [(x,y)|h <- [0..], x <- [0..h], y <- [0..h], h == x+y]

--Ejercicio 5

pitagóricas :: [(Integer, Integer, Integer)]
pitagóricas = [(a, b, c) | a <- [1..], b <-[1..], c <- [1..], a^2 + b^2 == c^2]

{-- Esta función se cuelga porq haskell toda la lista infinita de a y nunca llega a evaluar los valores de b ni de c--}


pitagoricas :: [(Integer,Integer,Integer)]
pitagoricas = [(a,b,c) | c <-[1..],b<-[1..c],a <-[1..c], a^2 + b^2 == c^2]


--Ejercicio 6

listasQueSuman :: Int -> [[Int]]
listasQueSuman 0 = [[]]
listasQueSuman n = concat [map ((:) x) (listasQueSuman (n-x)) | x <- [1..n]]


--Ejercicio 7 ⋆
--Definir en Haskell una lista que contenga todas las listas finitas de enteros positivos (esto es, con elementos mayores o iguales que 1).

listasFinitasDePositivos :: [[Int]]
listasFinitasDePositivos = concat[listasQueSuman n | n <-[1..]]


--Ejercicio 8 ⋆
--i. Definir utilizando map y filter:
--a) Una función que dada una lista de palabras devuelve una lista con aquellas que tienen menos de 5 letras.

menosDe5 :: [String] -> [String]
menosDe5 = filter (((flip (<)) 5).length)

--b) Una función que dada una lista de notas devuelve una lista de booleanos que indiquen si la nota está aprobada (es mayor a 6)

aprobados :: [Int] -> [Bool]
aprobados = map ((flip (>)) 6)

--c) Una función que dada una lista de números devuelve una lista que contiene solo los números pares elevados al cuadrado.

soloParesAlCuadrado ::Integral a => [a] -> [a] 
soloParesAlCuadrado l = map ((flip(^))2) soloPares
                    where soloPares = filter (((==)0).(flip mod) 2) l

--RECORDAR BIEN COMO FUNCIONA MOD: mod 2 4 = 2; mod 3 4 = 3; es decir, con mod x y estoy calculando el resto de calcular x / y

-- ii. Redefinir usando foldr las funciones sum, elem, (++), filter y map.

sum1 :: Num a => [a] -> a 
sum1 = foldr (+) 0 

elem1 :: Eq a => a -> [a] -> Bool
elem1 x = foldr ((||).((==)x)) False

masmas :: [a] -> [a] -> [a]
masmas = (\l1 l2 -> foldr (:) l2 l1)

filter1 :: (a -> Bool) -> 
