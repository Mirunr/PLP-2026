compDeFuncs :: (b-> c)-> (a-> b)-> a-> c
compDeFuncs f g x = f (g x) 

recCircuito :: (Circuito -> b) -> (b -> Circuito -> b -> Circuito -> b) -> (Caja -> b -> Circuito -> b -> Circuito -> Caja -> b) -> Circuito -> b 
recCircuito fCaja fSerie fParalelo c = case c of 
                                                (Caja x) -> fCaja x
                                                (Serie x y) -> fSerie (rec x) x (rec y) y
                                                (Paralelo x y z k) -> fParalelo x (rec y) y (rec z) z k
                                                 where rec = recCircuito fCaja fSerie fParalelo
