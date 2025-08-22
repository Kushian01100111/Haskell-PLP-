data AB a = Nil | Bin (AB a) a (AB a)
  deriving (Show, Eq)

valorAbsoluto :: Float -> Float
valorAbsoluto x =
  if x < 0
    then -x
    else x

bisiesto :: Int -> Bool
bisiesto n = mod n 4 == 0 && n /= 1000

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

divisores :: Int -> Int -> Int
divisores 0 m = divisores (1) m
divisores n m =
  if n == m
    then 0
    else
      if mod m n == 0
        then 1 + divisores (n + 1) m
        else divisores (n + 1) m

divisoresPrimos :: Int -> Int -> Int
divisoresPrimos 0 m = divisoresPrimos (1) m
divisoresPrimos n m =
  if n == m
    then 0
    else
      if mod m n == 0 && divisores 0 n == 1
        then 1 + divisoresPrimos (n + 1) m
        else divisoresPrimos (n + 1) m

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos = divisoresPrimos 0

-- >>> cantDivisoresPrimos 14
-- 2

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso n = Just (-n)

---------------------------
aEntero :: Either Int Bool -> Int
aEntero (Left n) = n
aEntero (Right n) =
  if n == True
    then 1
    else 0

----------------------------
longitud :: (Eq a) => [a] -> Float
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

limpiar :: String -> String -> String
limpiar _ [] = ""
limpiar x (y : ys) =
  if y `elem` x
    then limpiar x ys
    else y : limpiar x ys

----------------
suma :: [Float] -> Float
suma [x] = x
suma (x : xs) = x + suma xs

promedio :: [Float] -> Float
promedio xs = suma xs / longitud xs

difPromedio :: [Float] -> [Float]
difPromedio xs = difAux xs (promedio xs)

difAux :: [Float] -> Float -> [Float]
difAux [] _ = []
difAux (x : xs) avg = (x - avg) : difAux xs avg

----------------

todosIguales :: [Int] -> Bool
todosIguales [x] = True
todosIguales (x : y : xs)
  | x == y = todosIguales (y : xs)
  | otherwise = False

----------------

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

ej1 :: AB Int
ej1 = Nil

ej2 :: AB Int
ej2 = Bin Nil 0 Nil

ej3 :: AB Int
ej3 = Bin ej2 10 Nil

----------------
negacion :: Bool -> Bool
negacion m = not (m == True)

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) = Bin (negacionAB i) (negacion r) (negacionAB d)

tp1 :: AB Bool
tp1 = Bin (Bin Nil True Nil) False (Bin Nil True Nil)

----------------

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin i r d) = r * productoAB i * productoAB d

yp1 :: AB Int
yp1 = Bin (Bin Nil 2 Nil) 1 (Bin Nil 90 Nil)