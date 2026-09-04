module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

{--
data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito
--}

recCircuito :: (Caja -> b) -> (b -> b -> Circuito -> Circuito -> b) -> (Circuito -> b -> Circuito
recCircuito fCaja fSerie fParalelo c = case c of Caja -> 
                                                 Serie ->
                                                 Paralelo -> 


recCircuito = undefined -- TODO: COMPLETAR

-- 2: foldCircuito

foldCircuito = undefined -- TODO: COMPLETAR

-- 3 invertido

invertido = undefined -- TODO: COMPLETAR

-- 4: hayCaminoIluminado

hayCaminoIluminado = undefined -- TODO: COMPLETAR

-- 5: cantidadPrendidas

cantidadPrendidas = undefined -- TODO: COMPLETAR

-- 6: cajasDeCircuito

cajasDeCircuito = undefined -- TODO: COMPLETAR

-- 7: esCircuitoProlijo

esCircuitoProlijo = undefined -- TODO: COMPLETAR

-- 8: circuitoEmprolijado

circuitoEmprolijado = undefined -- TODO: COMPLETAR

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura = undefined -- TODO: COMPLETAR

-- 10: subCircuitoMásResistente

subCircuitoMásResistente = undefined -- TODO: COMPLETAR
{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

------

Demostrar paratodo Circuito:

Sea el predicado unario paratodo x :: Circuito 
       P(x) == alternado . alternado = id            # definimos esto como P(x) pero no se usa x tdvia, no habria q usar la ext primero tal vez?

Por extensionalidad funcional, basta ver:        
       paratodo x:: Circuito. 
              (alternado . alternado) x = id x

Entonces por Induccion Estructural sobre Circuito, se tiene que cumplir que:                                          
       paratodo a:: Caja.                                                                                              
              P(Caja a)
       paratodo c1:: Circuito. paratodo c2 :: Circuito. 
              P(c1) ^ P(c2) --> P(Serie c1 c2)                 --Sea P(c1) ^ P(c2) HI. Sea P(Serie c1 c2) TI
       paratodo c1:: Circuito. paratodo c2 :: Circuito. paratodo a1:: Caja. paratodo a2 :: Caja. 
              P(c1) ^ P(c2) --> P(Paralelo a1 c1 c2 a2)        --Sea P(c1) ^ P(c2) HI. Sea P(Paralelo a1 c1 c2 a2) TI


---------------
-  CASO BASE  -
---------------
paratodo a:: Caja. P(Caja a)

(alternado . alternado) Caja a = id Caja a
       Sea IZQ = (alternado . alternado) Caja a 
       Sea DER = id Caja a

IZQ : {C}  = alternado (alternado Caja a)
      {AC} = alternado (Caja (cajaAlternada a))
      {AC} = Caja (cajaAlternada (cajaAlternada a))

Por lema de generación sobre Caja, sabemos que, paratodo bool :: Bool
       a = Bombilla bool o bien a = Nada

Entonces paratodo a:: Caja, probemos cada caso en particular

       * Sea a = Nada, 
              IZQ   = Caja (cajaAlternada (cajaAlternada Nada))
              {CAN} = Caja (cajaAlternada(Nada))
              {CAN} = Caja Nada

       Vemos el lado Derecho
              DER = id Caja Nada
              {I} = Caja Nada

       Sea IZQ = DER queda probado el CASO BASE cuando a:: Caja, sea a = Nada

       * Sea a = Bombilla bool
              IZQ   = Caja (cajaAlternada (cajaAlternada Bombilla bool))
              {CAB} = Caja (cajaAlternada (Bombilla not bool))
              {CAB} = Caja (Bombilla not (not bool))

              Por lema de generación de booleanos, bool puede:                      # será que podemos usar q la propiedad ya está probada sobre booleanos en la teórica 
                     bool = True                                                    # que dice que not(not x) = x  ??
                     bool = False                                                   # también pienso, como esto es literal algo q aparece en la teórica,
                                                                                    # no será que hay q plantear en un apartado q probamos que not (not x) = x por inducción sobre booleanos
                     * Sea bool = True                                              # y entonces Caja (Bombilla not (not bool)) = Caja Bombilla bool
                            IZQ  = Caja (Bombilla not (not True))
                            {NT} = Caja (Bombilla not (False))
                            {NF} = Caja (Bombilla True)

                     Vemos el lado Derecho
                            DER = id Caja (Bombilla True)
                            {I} = Caja (Bombilla True)

                     Sea IZQ = DER queda probado el CASO BASE cuando a:: Caja, sea a = Bombilla True

                     * Sea bool = False
                            IZQ  = Caja (Bombilla not (not False))
                            {NF} = Caja (Bombilla not (True))
                            {NT} = Caja (Bombilla False)

                     Vemos el lado Derecho
                            DER = id Caja (Bombilla False)
                            {I} = Caja (Bombilla False)

                     Sea IZQ = DER queda probado el CASO BASE cuando a:: Caja, sea a = Bombilla False

Queda entonces demostrado el CASO BASE


--------------------
-  CASO INDUCTIVO  -
--------------------
* paratodo c1:: Circuito. paratodo c2 :: Circuito. 
              P(c1) ^ P(c2) --> P(Serie c1 c2)

       P(c1) = (alternado . alternado) c1 = id c1
       P(c2) = (alternado . alternado) c2 = id c2

       (alternado . alternado) (Serie c1 c2) = id (Serie c1 c2)
              Sea IZQ = (alternado . alternado) (Serie c1 c2)
              Sea DER = id (Serie c1 c2)

       DER :  {I} Serie c1 c2

       IZQ :  {C}  = alternado (alternado (Serie c1 c2))
              {AS} = alternado (Serie (alternado c1) (alternado c2))
              {AS} = Serie (alternado (alternado c1) alternado (alternado c2))
              {C}  = Serie ((alternado . alternado) c1) (alternado (alternado c2))
              {C}  = Serie ((alternado . alternado) c1) ((alternado . alternado) c2)              # acá no habría q reemplazar por id c1 y id c2 
                                                                                                  # y después hacer {I} y pasar a ci y cd
       Entonces por HI : paratodo c1:: Circuito. paratodo c2 :: Circuito. 
                                   P(c1) ^ P(c2) --> P(Serie c1 c2)

       Vemos que vale P(c1) ^ P(c2) entonces:

       Se cumple por TI que paratodo c1:: Circuito. paratodo c2 :: Circuito. 
                                   P(Serie c1 c2)

Sea IZQ = DER queda probado el CASO INDUCTIVO cuando P(Serie c1 c2)

* paratodo c1:: Circuito. paratodo c2 :: Circuito. paratodo a1:: Caja. paratodo a2 :: Caja. 
              P(c1) ^ P(c2) --> P(Paralelo a1 c1 c2 a2)

       P(c1) = (alternado . alternado) c1 = id c1
       P(c2) = (alternado . alternado) c2 = id c2

       (alternado . alternado) (Paralelo a1 c1 c2 a2) = id (Paralelo a1 c1 c2 a2)
              Sea IZQ = (alternado . alternado) (Paralelo a1 c1 c2 a2)
              Sea DER = id (Paralelo a1 c1 c2 a2)
       
       DER :  {I} Paralelo a1 c1 c2 a2

       IZQ :  {C}  = alternado (alternado (Paralelo a1 c1 c2 a2))
              {AP} = alternado (Paralelo (cajaAlternada a1) (alternado c1) (alternado c2) (cajaAlternada a2))
              {AP} = Paralelo (cajaAlternada(cajaAlternada a1)) (alternado (alternado c1)) (alternado (alternado c2)) (cajaAlternada(cajaAlternada a2))
              {C}  = Paralelo ((cajaAlternada . cajaAlternada) a1) (alternado (alternado c1)) (alternado (alternado c2)) (cajaAlternada(cajaAlternada a2))
              {C}  = Paralelo ((cajaAlternada . cajaAlternada) a1) ((alternado . alternado) c1) (alternado (alternado c2)) (cajaAlternada(cajaAlternada a2))
              {C}  = Paralelo ((cajaAlternada . cajaAlternada) a1) ((alternado . alternado) c1) ((alternado . alternado) c2) (cajaAlternada(cajaAlternada a2))
              {C}  = Paralelo ((cajaAlternada . cajaAlternada) a1) ((alternado . alternado) c1) ((alternado . alternado) c2) ((cajaAlternada . cajaAlternada) a2) #no hay q hacer nada
                                                                                                                                                                 #con el calt.calt a1?
       Entonces por HI : paratodo c1:: Circuito. paratodo c2 :: Circuito.                                                                                                      
                                   P(c1) ^ P(c2) --> P(Paralelo a1 c1 c2 a2)

       Vemos que vale P(c1) ^ P(c2) entonces:

       Se cumple por TI que paratodo c1:: Circuito. paratodo c2 :: Circuito. 
                                   P(Serie c1 c2)

Sea IZQ = DER queda probado el CASO INDUCTIVO cuando P(Paralelo a1 c1 c2 a2)

Queda demostrado entonces paratodo x:: Circuito 
       P(x) == alternado . alternado = id

       
--}
