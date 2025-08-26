{-
Ej1. Considerar las siguientes definiciones de funciones
I. ¿Cuál es el tipo de cada función?. (Suponer que todos los números  son de tipo Float)

II. Indicar cuáles de las funciones anteriores no están currificads. Para cada una de ellas, definir la funcion currificada correspondiente. Recordar dar el tipo de la funcion.

max2:: Float -> (Float -> Float)
max2 (x,y) | x >= y     = x
           | otherwise  = y
Esta currificada

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
NO esta currificada???
predecesor x = x `subtract` 1

evaluarEnCero:: Num a =>(a -> b) -> b
evaluarEnCero = \f -> f 0
NO esta currificada

dosVeces:: (a -> a) -> a -> a
dosVeces = \f -> f . f
NO esta currficada

flipAll:: ([a] -> [b] -> [c]) -> [b] -> [a] -> [c]
flipAll = map flip
NO esta currificada

flip:: (a -> b -> c) -> b -> a -> c
flipRaro:: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
b puede ser el valor de entrada que recibira el segundo flip ?

-}