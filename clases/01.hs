{- Clase 1 - Primeras funciones -}

{-
  Ejercicios: Implementar las siguientes funciones, especificando su signatura.

  Observación: Cuando el problema en cuestión trata sobre números naturales,
  se puede simplemente usar el tipo Int e ignorar el comportamiento del
  programa si el usuario decide ejecutarlo usando para los parámetros enteros
  negativos o 0.
-}

{-
  01 - absoluto: calcula el valor absoluto de un número entero.
-}

absoluto :: Float -> Float
absoluto n
  | n >= 0 = n
  | otherwise = - n

{-
  02 - maximoAbsoluto: devuelve el máximo entre el valor absoluto de dos
  números enteros.
-}

maximo :: Float -> Float -> Float
maximo m n
  | m >= n = m
  | otherwise = n

maximoAbsoluto :: Float -> Float -> Float
maximoAbsoluto m n = maximo (absoluto m) (absoluto n)

{-
  03 - maximo3: devuelve el máximo entre tres números enteros.
-}

maximo3 :: Float -> Float -> Float -> Float
maximo3 m n p = maximo m $ maximo n p

{-
  04 - algunoEs0: dados dos números racionales, decide si alguno de los dos es
  igual a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).
-}

es0 :: (Num a, Eq a) => a -> Bool
es0 = (== 0)

algunoEs0 :: Float -> Float -> Bool
algunoEs0 m n = es0 m || es0 n

algunoEs0' :: Float -> Float -> Bool
algunoEs0' 0 _ = True
algunoEs0' _ n = es0 n

{-
  05 - ambosSon0: dados dos números racionales, decide si ambos son iguales a 0
  (hacerlo dos veces, una sin usar y otra usando pattern matching).
-}

ambosSon0 :: Float -> Float -> Bool
ambosSon0 m n = es0 m && es0 n

ambosSon0' :: Float -> Float -> Bool
ambosSon0' 0 0 = True
ambosSon0' _ _ = False

{-
  06 - esMultiploDe: dados dos números naturales, decidir si el primero es
  múltiplo del segundo.
-}

esMultiploDe :: Int -> Int -> Bool
esMultiploDe m n = es0 $ mod m n

{-
  07 - digitoUnidades: dado un número natural, extrae su dígito de las
  unidades.
-}

digitoUnidades :: Int -> Int
digitoUnidades n = mod n 10

{-
  08 - digitoDecenas: dado un número natural, extrae su dígito de las decenas.
-}

digitoDecenas :: Int -> Int
digitoDecenas n = digitoUnidades $ div n 10
