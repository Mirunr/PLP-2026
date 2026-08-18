data Dirección = Norte | Este | Sur | Oeste

opuesta :: Dirección -> Dirección 
opuesta Norte = Sur 
opuesta Sur = Norte 
opuesta Este = Oeste 
opuesta Oeste = Este

{--el´Ultimo´IndiceDe :: Eq a => a -> [a] -> Int
que dado un elemento x y una lista de elementos xs, devuelve el
´ındice de la ´ultima ocurrencia de x en xs.
Hacerla total usando el tipo Maybe --}

elUltimoIndiceDe :: Eq a => a -> [a] -> Maybe Int 
elUltimoIndiceDe _ [] = Nothing 
elUltimoIndiceDe n (x:xs) = 
