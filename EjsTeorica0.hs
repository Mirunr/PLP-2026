data Dirección = Norte | Este | Sur | Oeste

opuesta :: Dirección -> Dirección 
opuesta Norte = Sur 
opuesta Sur = Norte 
opuesta Este = Oeste 
opuesta Oeste = Este

{-- elUltimoIndiceDe :: Eq a => a -> [a] -> Int
que dado un elemento x y una lista de elementos xs, devuelve el
índice de la última ocurrencia de x en xs.
Hacerla total usando el tipo Maybe --}

elUltimoIndiceDe :: Int -> [Int] -> Maybe Int 
elUltimoIndiceDe n lista = if null (listaDeApConIndice n 0 lista) then Nothing else Just (maximum (listaDeApConIndice n 0 lista))

listaDeApConIndice :: Int -> Int -> [Int] -> [Int]
listaDeApConIndice _ _ [] = []
listaDeApConIndice elem iAct (x:xs) = if elem == x then iAct : listaDeApConIndice elem (iAct+1) xs else listaDeApConIndice elem (iAct+1) xs

data AB a = Nil | Bin (AB a) a (AB a)

preorder :: AB a -> [a]
preorder Nil = []
preorder (Bin i r d) =  [r] ++ preorder i ++ preorder d 

inorder :: AB a -> [a] 
inorder Nil = []
inorder (Bin i r d) = inorder i ++ [r] ++ inorder d 

postorder :: AB a -> [a]
postorder Nil = []
postorder (Bin i r d) = postorder d ++ postorder i ++ [r]

data Conj a = CConj [a] deriving Show

vacio :: Conj a
vacio = CConj []

insertar :: Eq a => a-> Conj a-> Conj a
insertar e (CConj c) = if elem e c then CConj c else CConj (e:c)

pertenece :: Eq a => a -> Conj a -> Bool
pertenece e (CConj c) = elem e c 

eliminar :: Eq a => a -> Conj a -> Conj a
eliminar e (CConj c) = CConj (quitar e c)

quitar :: Eq a => a -> [a] -> [a]
quitar e [] = []
quitar e (x:xs) = if e == x then xs else e : quitar e xs

{--data Dict k v = CDict (AB (k, v)) deriving Show 

vacio1 :: Dict k v
vacio1 = CDict Nil--}

{--subsecuencias :: [a] -> [[a]]
subsecuencias [x] = [x]
subsecuencias (x:xs) = [x] : subsecuencias xs : --}