{- Clase 9 - Algoritmos sobre enteros I -}

module Clases.Clase09 where

import Clases.Clase01 (esMultiploDe)
import Clases.Clase06 (maximo)
import Clases.Clase07 (Set, interseccion)

{-
  01 - Definir la función digitos :: Integer -> Integer -> [Integer] que,
  dados n>=0 y b>1, retorne su representación por listas en base b.
-}

digitos :: Integer -> Integer -> [Integer]
digitos 0 _ = [0]
digitos n b = digitos' n b

digitos' :: Integer -> Integer -> [Integer]
digitos' 0 b = []
digitos' 1 b = [1]
digitos' n b = digitos' (div n b) b ++ [mod n b]

{-
  02 - Definir la función numero :: [Integer] -> Integer -> Integer que, dada
  la representación por listas de n>=0 en base b y la base b>1, retorne n.
-}

numero :: [Integer] -> Integer -> Integer
numero [n] b = n
numero (n : ns) b = n * b ^ (length (n : ns) - 1) + numero ns b

{-
  03 - Escribir la función divisores :: Int -> Set Int que dado un valor n!=0
  retorna el conjunto de sus divisores positivos.
-}

divisores :: Int -> Set Int
divisores n = divisoresDesde n $ abs n

divisoresDesde :: Int -> Int -> Set Int
divisoresDesde n 1 = [1]
divisoresDesde n k
  | n `esMultiploDe` k = k : divisoresDesde n (k - 1)
  | otherwise = divisoresDesde n $ k - 1

{-
  04 - Completar la función mcdDef, definiendo las funciones restantes.
-}

mcdDef :: Int -> Int -> Int
mcdDef a 0 = abs a
mcdDef 0 b = abs b
mcdDef a b = maximo $ interseccion (divisores a) (divisores b)

{-
  06 - Definir la función mcd :: Int -> Int -> Int que dados a,b ∈ Z, b!=0,
  calcule (a:b) usando el algoritmo de Euclides.
-}

mcd :: Int -> Int -> Int
mcd a b | a < b = mcd b a
mcd a 0 = abs a
mcd a b = mcd b $ mod a b

{-
  08 - Definir un función mcm :: Int -> Int -> Int que dados a>=0 y b>=0
  calcule el mínimo d>=0 que sea múltiplo tanto de a como de b.
  ¿Cuánto vale mcm 0 0?
-}

mcm :: Int -> Int -> Int
mcm a b = head $ [x | x <- [a, a * 2 ..], x `esMultiploDe` b]

{-
  09 - Programar la función emcd :: Int -> Int -> (Int,Int,Int) que, dados
  a y b, utilice el algoritmo de Euclides extendido para obtener una tripla
  ((a:b),s,t) tal que sa+tb = (a:b)
-}

mcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
mcdExt a b | a < b = mcdExt b a
mcdExt a 0 = (a, 1, 0)
mcdExt a b = (d, t, s - t * q)
  where
    (q, r) = (div a b, mod a b)
    (d, s, t) = mcdExt b r

{-
  10 - Definir una función que dados a!=0 y b!=0 encuentre el par s,t ∈ Z tal
  que sa+tb = (a:b) donde s>=0 sea lo mínimo posible.
  Repasar la teórica para este ejercicio.
-}

mcdExt' :: Integer -> Integer -> (Integer, Integer)
mcdExt' a b | a < b = mcdExt' b a
mcdExt' a 0 = (1, 0)
mcdExt' a b = (t, s - t * q)
  where
    (q, r) = (div a b, mod a b)
    (s, t) = mcdExt' b r