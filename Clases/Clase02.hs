{- Clase 2 - Primeras funciones II -}

{-
  Ejercicios: Implementar las siguientes funciones, especificando su signatura.
  Usar pares para representar vectores.

  Observación: Chequear qué tipo asigna Haskell cuando se elimina la signatura
  y argumentar las razones de cada tipo.
-}

module Clases.Clase02 where

import Clases.Clase01
import Data.List

{-
  01 - estanRelacionados: dados dos números reales, decide si están
  relacionados considerando la relación de equivalencia en R cuyas clases de
  equivalencia son: (−∞, 3], (3, 7] y (7,∞).
-}

type R = Float

type R2 = (R, R)

estanRelacionados :: R2 -> Bool
estanRelacionados (a, b) = relacion menor3 || relacion entre3y7 || relacion mayor7
  where
    relacion clase = all clase [a, b]
    menor3 = (<= 3)
    entre3y7 n = n > 3 && n <= 7
    mayor7 = (> 7)

{-
  02 - prodInt: calcula el producto interno entre dos vectores de R2.
-}

prodInt :: R2 -> R2 -> R
prodInt (a, b) (c, d) = a * c + b * d

{-
  03 - todoMenor: dados dos vectores de R2, decide si es cierto que cada
  coordenada del primer vector es menor a la coordenada correspondiente del
  segundo vector.
-}

todoMenor :: R2 -> R2 -> Bool
todoMenor (a, b) (c, d) = a < c && b < d

{-
  04 - distanciaPuntos: calcula la distancia entre dos puntos de R2.
-}

distanciaPuntos :: R2 -> R2 -> R
distanciaPuntos (a, b) (c, d) = norma p
  where
    p = (a - c, b - d)
    norma (m, n) = sqrt $ sum $ map (^ 2) [m, n]

{-
  05 - sumaTerna: dada una terna de enteros, calcula la suma de sus tres
  elementos.
-}

type Terna = (Int, Int, Int)

sumaTerna :: Terna -> Int
sumaTerna (a, b, c) = a + b + c

{-
  06 - posicPrimerPar: dada una terna de enteros, devuelve la posición del
  primer número par si es que hay alguno, y devuelve 4 si son todos impares.
-}

esPar :: (Integral a) => a -> Bool
esPar n = esMultiploDe n 2

posicPrimerPar :: Terna -> Int
posicPrimerPar (a, b, c) =
  case findIndex esPar [a, b, c] of
    Just i -> i + 1
    Nothing -> 4

{-
  07 - crearPar :: a -> b -> (a, b): crea un par a partir de sus dos
  componentes dadas por separado (debe funcionar para elementos de cualquier tipo).
-}

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

{-
  08 - invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como
  parámetro (debe funcionar para elementos de cualquier tipo).
-}

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)