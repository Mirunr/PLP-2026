data Dirección = Norte | Este | Sur | Oeste

opuesta :: Dirección -> Dirección 
opuesta Norte = Sur 
opuesta Sur = Norte 
opuesta Este = Oeste 
opuesta Oeste = Este

{--elUltimoIndiceDe :: Eq a => a -> [a] -> Int
que dado un elemento x y una lista de elementos xs, devuelve el
índice de la última ocurrencia de x en xs.
Hacerla total usando el tipo Maybe --}

elUltimoIndiceDe :: Eq a => a -> [a] -> Maybe Int 
elUltimoIndiceDe _ [] = Nothing 
elUltimoIndiceDe n (x:xs) = 

indiceSiguiente :: Eq a, Num b => a -> b -> [a] -> b
indiceSiguiente x iact [] = iact
indiceSiguiente x iact (e:es) = if x == e then iact else indiceSiguiente x iact+1 es 
