{- Clase 6 - Listas -}

module Clases.Clase06 where

import Clases.Clase02 (esPar)

{-
  01 - productoria :: [Int] -> Int que devuelve la productoria de los
  elementos.
-}

productoria :: [Int] -> Int
productoria [] = 1
productoria (x : xs) = x * productoria xs

{-
  02 - sumarN :: Int -> [Int] -> [Int] que dado un número N y una lista xs,
  suma N a cada elemento de xs.
-}

sumarN :: Int -> [Int] -> [Int]
sumarN _ [] = []
sumarN n (x : xs) = (n + x) : sumarN n xs

{-
  03 - sumarElPrimero :: [Int] -> [Int] que dada una lista no vacía xs, suma el
  primer elemento a cada elemento de xs.
  Ejemplo sumarElPrimero [1,2,3] ~> [2,3,4]
-}

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

{-
  04 - sumarElUltimo :: [Int] -> [Int] que dada una lista no vacía xs, suma el
  último elemento a cada elemento de xs.
  Ejemplo sumarElUltimo [1,2,3] ~> [4,5,6]
-}

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (last xs) xs

{-
  05 - pares :: [Int] -> [Int] que devuelve una lista con los elementos pares
  de la lista original.
  Ejemplo pares [1,2,3,5,8] ~> [2,8]
-}

pares :: [Int] -> [Int]
pares [] = []
pares (x : xs)
  | esPar x = x : pares xs
  | otherwise = pares xs

{-
  06 - quitar :: Int -> [Int] -> [Int] que elimina la primera aparición del
  elemento en la lista (de haberla).
-}

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar e (x : xs)
  | e == x = xs
  | otherwise = x : quitar e xs

{-
  07 - quitarTodas :: Int -> [Int] -> [Int] que elimina todas las apariciones
  del elemento en la lista (de haberla).
-}

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas e (x : xs)
  | e == x = quitarTodas e xs
  | otherwise = x : quitarTodas e xs

{-
  08 - hayRepetidos :: [Int] -> Bool que indica si una lista tiene elementos
  repetidos.
-}

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x : y : xs) = x == y || hayRepetidos (y : xs)

{-
  09 - eliminarRepetidosAlFinal :: [Int] -> [Int] que deja en la lista la
  primera aparición de cada elemento, eliminando las repeticiones adicionales.
-}

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (x : xs) = x : eliminarRepetidosAlFinal (quitarTodas x xs)

{-
  10 - eliminarRepetidosAlInicio :: [Int] -> [Int] que deja en la lista la
  última aparición de cada elemento, eliminando las repeticiones adicionales.
-}

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio xs = eliminarRepetidosAlInicio (quitarTodas lst rst) ++ [lst]
  where
    lst = last xs
    rst = init xs

{-
  11 - maximo :: [Int] -> Int que calcula el máximo elemento de una lista no
  vacía.
-}

maximo :: [Int] -> Int
maximo [] = 0
maximo [x] = x
maximo (x : y : xs)
  | x > y = maximo $ x : xs
  | otherwise = maximo $ y : xs

{-
  12 - ordenar :: [Int] -> [Int] que ordena los elementos de forma creciente.
-}

ordenar :: [Int] -> [Int]
ordenar [x] = [x]
ordenar xs = ordenar (quitar maxi xs) ++ [maxi]
  where
    maxi = maximo xs

{-
  13 - reverso :: [Int] -> [Int] que dada una lista invierte su orden.
-}

reverso :: [Int] -> [Int]
reverso [] = []
reverso [x] = [x]
reverso (x : xs) = reverso xs ++ [x]

{-
  14 - concatenar :: [Int] -> [Int] -> [Int] que devuelve la concatenación de
  la primera lista con la segunda.
  Ejemplo concatenar [1,2,3] [4,5,6] ~> [1,2,3,4,5,6],
  concatenar [] [4,5,6] ~> [4,5,6].
  Esta operación está en el prelude y se escribe como (++).
-}

concatenar :: [Int] -> [Int] -> [Int]
concatenar xs ys = xs ++ ys

{-
  15 - zipi :: [a] -> [b] -> [(a,b)] que devuelve una lista de tuplas, cada
  tupla contiene elementos de ambas listas que ocurren en la misma posición.
  En caso que tengan distintas longitudes, la longitud de la lista resultado es
  igual a la longitud de la lista más chica pasada por parámetro.
  Ejemplo zipi [1,2,3] ['a','b','c'] ~> [(1,'a'), (2,'b'), (3,'c')],
  zipi [1,2,3] ['a','b'] ~> [(1,'a'), (2,'b')].
  Esta operación está en el prelude y se escribe como zip.
-}

zipi :: [a] -> [b] -> [(a, b)]
zipi = zip