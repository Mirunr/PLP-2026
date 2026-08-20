compDeFuncs :: (b-> c)-> (a-> b)-> a-> c
compDeFuncs f g x = f (g x) 

