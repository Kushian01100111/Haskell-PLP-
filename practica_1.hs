import Data.List (tails, nub)

{-
Ej1. Considerar las siguientes definiciones de funciones
I. ¿Cuál es el tipo de cada función?. (Suponer que todos los números  son de tipo Float)

II. Indicar cuáles de las funciones anteriores no están currificads. Para cada una de ellas, definir la funcion currificada correspondiente. Recordar dar el tipo de la funcion.

max2:: Float -> (Float -> Float)
max2 (x,y) | x >= y     = x
           | otherwise  = y
No es currificada
max2 x y  | x >= y       = x
          | otherwise   = y

normaVectorial:: (Float -> Float ) -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)
NO esta currficada
==> Version currificada
normaVectorial x y = sqrt(x^2 + y^2)

((-1) 2)

-:: Num a => a -> (a -> a)

subtract:: Num a => a -> (a -> a)
subtract = flip (-)
Esta currificada

predecesor:: Num a => (a -> a)
predecesor = subtract 1
Es currificada ya que utiliza una funcion currificada, es decir que le paso n parametro y devuelve una funcion que me regresa un m del mismo tipo

evaluarEnCero:: Num a =>(a -> b) -> b
evaluarEnCero = \f -> f 0

evaluarEnCero :: a -> ((a -> b) -> b)
evaluarEnCero f = f 0
Es currificada

dosVeces:: (a -> a) -> (a -> a)
dosVeces = \f -> f . f
Es currificada

flipAll:: ([a] -> [b] -> [c]) -> [b] -> [a] -> [c]
flipAll = map flip
NO esta currificada

flip:: (a -> b -> c) -> b -> a -> c
flipRaro:: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
b puede ser el valor de entrada que recibira el segundo flip ?

-}
{-
    Ejercicio 2
    I. Definir la funcion curry, que dada una funcion de dos argumentos, devuelve su equivalente currificada
    II. Definr la funcion uncurry, que dada una funcion currificada de dos argumentos, devuelve su version no currificada equivalente
    III. ¿Se podría definir una funcion curryN, que tome una funcion de un numero arbitrario de argumentos y devuelva su version currificada?
-}
curry :: ((a, b) -> c) -> (a -> (b -> c))
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y

{- curryN no de ser posible en haskell ya las funciones de se contruyen a partir de pattern matching y el hecho de curry o uncurry una funcion depende justamente de yo puede manipular (sin modificarlos) los argumentos los cuales son pasados a curry y luego asigno los argumentos de la funcion que quiero currificar.

Seria absurdo de realizar una funcion asi, con el desconocimiento en cuantos argumentos tendra mi funcion en el paradigma funcional
((a,b,c,d,..., n) -> c) -> (a-> ( b -> (c -> (b ->(...->(n))))))

 -}

{- Esquemas de recursión
    Ej3
    I. Redefinir usando foldr las funciones sum, elem, (++), filter, map.

    II. Definir la funcion mejorSegun, que devuelva el maximo elemento de la lista según una función de comparacion, utilizando foldr1.

    III. Definir la funcion sumasParciales, que dada una lista de numeros devuelve otra de la misma longitud, que tiene en cada posicion la suma parcial de los elementos de la lista original desde la cabeza hasta la posción actual. Por ejemplo
-}

-- I
foldrr :: (a -> b -> b) -> b -> [a] -> b
foldrr _ z [] = z
foldrr f z (x : xs) = f x (foldrr f z xs)

{-
    f 1 (foldrr f id [2]) [2,3]
    1 : 2 : llamada recursiva [2] [3]
-}

sum = foldrr (+) 0

elem y = foldrr (\x r -> x == y || r) False

-- (++) _ [] = []
-- (++) xs (y:ys) = xs:y ys
-- (++) xs ys = foldrr (:) ys xs

filter fCond = foldrr (\x r -> if fCond x then x else r) []


-- II

{-
max [x] = x
max (x:y:xs) =
    if x > y
        then max (x:xs)
        else max (y:xs)
-}
foldrr1 f [x] = x
foldrr1 f (x : xs) = f x (foldr1 f xs)

mejorSegun cond = foldrr1 (\x r -> if cond x r then x else r)

-- III

{-
sumaParciales [] = []
sumaParciales (x:y:xs) = x : sumaParciales (x+y:xs)
-}

sumasParciales :: (Num a) => [a] -> [a]
sumasParciales = foldl (\acc x -> acc ++ if null acc then [x] else [x + last acc]) []

-- IV

sumaAlt :: [Int] -> Int
sumaAlt = foldrr (\x r -> -r + x) 0

-- V
sumaAltInv :: [Int] -> Int
sumaAltInv = foldl (\acc x -> -acc + x) 0

{-
Ejercicio 4
I. Definir la funcion permutaciones:: [a] -> [[a]], que dada una lista devuelve todas las permutaciones. Se recomienda utilizar concatMap, y tambien take y drop

-}

{-
    Ejecicio 5, Considerar las siguientes funciones
    Indicar si la recurcion de utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr. En caso contrario, explicar el motivo

-}

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x : xs) =
  if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)

{-
    NO es una funcion con recurcion estructural, que se hacen varias llamadas a la variable xs sin g(elementosEnPosicionesPares) lo cual es una de la cosas que definen una recursion estructural.
-}

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x : xs) = \ys ->
  if null ys
    then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

entrelazarr :: [a] -> [a] -> [a]
entrelazarr = foldrr f id
  where
    f x rec = \ys -> case ys of
      [] -> x : rec []
      (y : yss) -> x : y : rec yss

{-
    Ejercicio 6
    El siguente esquema captura la recursion primitiva sobre listas
    a. Definir la funcion sacarUna:: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el resultado de eliminar de la lista la primera aparicion del elemento (si esta presente)
-}

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y =
  recr (\ x xs rec -> if y == x 
                      then xs
                      else x : rec ) []

insertarOrdenado:: Ord a => a -> [a] -> [a]
insertarOrdenado y =
  recr (\ x xs rec -> if y <= x  
                      then y:x:xs
                      else if x <= y && null xs
                        then x:y:xs
                      else x : rec) []

mapPares::(a -> b -> (a, b)) -> [(a,b)] -> [(a,b)]
mapPares f = foldrr (\ x rec -> case x of 
                                (xs, xy) -> f xs xy : rec) []  

data Nat = Zero 
         | Succ Nat

foldNat:: Integer 
        -> (Integer -> Integer)
        -> Nat
        -> Integer
foldNat fzero fsucc expr = 
  case expr of
    Zero -> fzero
    Succ a -> fsucc (foldNat fzero fsucc a) 

data Polinomio a
  = X
  | Cte a
  | Suma (Polinomio a) (Polinomio a)
  | Prod (Polinomio a) (Polinomio a)
  deriving (Show)

foldPoli::  b
          ->(a -> b)
          ->(b -> b -> b)
          ->(b -> b -> b)
          -> Polinomio a 
          -> b
foldPoli x fconst fsuma fprod expr = 
  case expr of
    X -> x 
    Cte a -> fconst a 
    Suma a b -> fsuma (foldPoli x fconst fsuma fprod a) (foldPoli x fconst fsuma fprod b) 
    Prod a b -> fprod (foldPoli x fconst fsuma fprod a) (foldPoli x fconst fsuma fprod b)

evaluar :: Num a => a -> Polinomio a -> a 
evaluar x = foldPoli x id (+) (*)  


data AB a = Nil
          |Bin (AB a) a (AB a)

foldAB:: b -> 
        (b -> a -> b -> b) 
        -> AB a 
        -> b 
foldAB fnil fbin exprs = 
  case exprs of 
    Nil -> fnil
    (Bin a b c) -> fbin(foldAB fnil fbin a) b (foldAB fnil fbin c)


recrAB :: b ->
          (b -> AB a -> a -> b -> AB a -> b)
          -> AB a 
          -> b
recrAB fnil fbin exprs = 
  case exprs of 
    Nil -> fnil
    (Bin a b c) -> fbin (recrAB fnil fbin a) a b (recrAB fnil fbin c) c


altura:: AB a -> Integer
altura = foldAB 0 (\b a c -> 1 + max b c)

cantNodos:: AB a -> Integer
cantNodos = foldAB 0 (\b a c -> 1 + b + c) 


mejorSegunAB::(a -> a -> Bool) -> AB a -> a 
mejorSegunAB fcond = recrAB (error "Nil") (\ri i r rd d -> comparacion fcond (comparacion fcond r ri i) rd d) 

comparacion:: (a -> a -> Bool) -> a -> a  -> AB a -> a
comparacion fcond r i rec = 
  case rec of 
    Nil -> r
    _ -> if fcond r i then r else i 


raiz:: AB a -> a 
raiz (Bin a r b) = r 

esABB:: Ord a => AB a -> Bool 
esABB = recrAB True (\ri i r rd d -> (r >= (raiz i)) && (r < (raiz d)) && (ri && rd))

{-
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f xs = actualizarElem' f xs n


actualizarElem' :: (a -> a) -> [a] -> Int -> [a]
actualizarElem' f = foldr (\ x rec i -> if i == 0 then f x rec (i-1)
                                        else x  : rec (i-1)) (const [])
-}

comienzoDeIntervalos i n t=  -(1/0) : (Prelude.map (\x -> i + x*t))  [0..n-2]
finesDeIntervalos i n t  = (Prelude.map (\x -> (i + x*t) - 1))  [0..n-2] ++ [(1/0)]


{-



data FS = Arch String |  Dir String [FS] deriving (Eq, Show)

foldFS:: (String -> a) ->
         (String -> [a] -> a ) ->
          FS -> 
          a
foldFS fArc fDir fs = 
    case fs of 
      Arch a -> fArc a 
      Dir a xs -> fDir a (map (foldFS fArc fDir) xs) 


recrFS::(String -> a) ->
        (String -> [FS] -> [a] -> a) ->
        FS -> 
        a 
recrFS gArc gDir  fs = 
    case fs of 
      Arch a -> gArc a 
      Dir a xs -> gDir a xs (map (recrFS gArc gDir) xs)


rutas exprs = foldFS (:[]) (\ s rec -> map (\m -> s++"/"++m) (concat rec))

--valido:: FS -> Bool
--valido fs = rutas fs == nub (rutas fs)
-}

data LineaProd = Materiales [String] | Agregar String LineaProd| Unir LineaProd LineaProd

foldLP::([String] -> a) -> 
        (String -> a -> a) -> 
        (a -> a -> a ) -> 
        LineaProd -> 
        a
foldLP fMat fAgre fUni expre = 
  case expre of 
    Materiales xs -> fMat xs
    Agregar s prod -> fAgre s (foldLP fMat fAgre fUni prod)
    Unir prod1 prod2 -> fUni (foldLP fMat fAgre fUni prod1) (foldLP fMat fAgre fUni prod2)


recrLP::([String] -> a) -> 
        (String -> LineaProd -> a -> a) -> 
        (a -> LineaProd -> a -> LineaProd -> a) -> 
        LineaProd -> 
        a
recrLP gMat gAgre gUni expre = 
  case expre of 
    Materiales xs -> gMat xs
    Agregar s prod -> gAgre s prod (recrLP gMat gAgre gUni prod)
    Unir prod1 prod2 -> gUni (recrLP gMat gAgre gUni prod1) prod1 (recrLP gMat gAgre gUni prod2) prod2

{- l1 = unir
        (Materiales [])
        (Unir ) -}
materialesUsados:: LineaProd -> [String] 
materialesUsados expr = nub (foldLP (id) (:) (++) expr)


sublineasDisjuntas:: LineaProd -> Bool 
sublineasDisjuntas expr = recrLP (\_ -> True) (\_ _ rec-> rec)  (\i prod1 d prod2 ->
      if i && d 
        then (materialesUsados prod1)++(materialesUsados prod2) == nub((materialesUsados prod1)++(materialesUsados prod2))
        else False
        ) expr

estructura::LineaProd -> [String] 
estructura expr = foldLP 
  (\ _ -> ["Material"]) 
  (\_ rec -> "Agregar":rec) 
  (\ri rd -> ("Unir":ri)++("Unir":rd) ) 
  expr

mismaEstructura:: LineaProd -> LineaProd -> Bool
mismaEstructura expr1 expr2  = (estructura expr1) == (estructura expr2)


l1::LineaProd
l1 =
  Unir
    (Materiales ["acero", "cobre"])
    (Unir
      (Agregar "madera" (Materiales ["madera"]))
      (Materiales ["mercurio"])
    )

l2 :: LineaProd
l2 =
  Unir
    (Materiales ["acero", "cobre"])
    (Unir
      (Agregar "madera"   (Materiales ["aluminio"]))
      (Agregar "aluminio" (Materiales ["plástico"]))
    )

l3 :: LineaProd
l3 =
  Unir
    (Materiales ["m1"])
    (Unir
      (Agregar "m3" (Materiales ["m3", "m4"]))
      (Materiales ["m1", "m2"])
    )


data ABNV a = Hoja a | Uni a (ABNV a) | Bi (ABNV a)  a (ABNV a) deriving (Eq, Show)

foldABNV:: (b -> a ) ->
           (b -> a -> a) -> 
           (a -> b -> a -> a) ->
            ABNV b ->
            a  
foldABNV fHoja fUni fBi expre = 
  case expre of 
    Hoja r -> fHoja r 
    Uni r rec -> fUni r (foldABNV fHoja fUni fBi rec)
    Bi recA r recB -> fBi (foldABNV fHoja fUni fBi recA) r (foldABNV fHoja fUni fBi recB)


recrABNV:: (b -> a ) ->
           (b -> ABNV b -> a -> a) -> 
           (a -> ABNV b -> b -> a -> ABNV b  -> a) ->
            ABNV b ->
            a  
recrABNV fHoja fUni fBi expre = 
  case expre of 
    Hoja r -> fHoja r 
    Uni r rec -> fUni r rec (recrABNV fHoja fUni fBi rec) 
    Bi recA r recB -> fBi (recrABNV fHoja fUni fBi recA) recA r (recrABNV fHoja fUni fBi recB) recB


elemABNV:: Eq a => a -> ABNV a -> Bool 
elemABNV elem expre = foldABNV 
  (== elem) 
  (\r rec -> if rec then True else r == elem ) 
  (\recI r recD -> if recI || recD then True else r == elem) 
  expre 

reemplazarUno:: Eq a => a -> a -> ABNV a -> ABNV a 
reemplazarUno prev sub arbol = recrABNV 
  (\r -> if r == prev then Hoja sub else Hoja r)
  (\r ab rec -> if r == prev then Uni sub ab else Uni r rec )
  (\recI abI r recD abD -> 
    if r == prev 
      then Bi abI sub abD
      else if elemABNV prev abI
        then Bi recI r abD
      else if elemABNV prev abD
        then Bi abI r recD
      else Bi abI r abD
    ) arbol


nivel:: ABNV a -> Int -> [a] 
nivel = foldABNV 
  (\r n -> if n == 0 then [r] else [])
  (\r rec n -> if n == 0 then [r] else rec (n-1))
  (\recI r recD n -> if n == 0 then [r] else (recI (n-1))++(recD (n-1)))

abnv:: ABNV Int
abnv =
  Bi
    (Uni 3 (Hoja 1))
    3
    (Bi
      (Hoja 2)
      5
      (Uni 2 (Hoja 7)))