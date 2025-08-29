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

sum = foldrr (+) 0

elem y = foldrr (\x r -> x == y || r) False

-- (++) _ [] = []
-- (++) xs (y:ys) = xs:y ys
-- (++) xs ys = foldrr (:) ys xs

filter fCond = foldrr (\x r -> if fCond x then x else r) []

map cond = foldrr (\x r -> cond x) []

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

{-Consultar en clase porque no entiendo el funcionamiento de foldl-}