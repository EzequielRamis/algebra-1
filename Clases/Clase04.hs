{- Clase 4 - Sumatorias -}

module Clases.Clase04 where

import Clases.Clase02 (esPar)
import Clases.Clase03 (digitosIguales)
import GHC.Natural (Natural)

operatoria :: (Integral b) => (a -> a -> a) -> b -> b -> (b -> a) -> a
operatoria o i n f
  | i == n = f n
  | otherwise = operatoria o i (n - 1) f `o` f n

sumatoria :: (Num a, Eq a) => Natural -> Natural -> (Natural -> a) -> a
sumatoria = operatoria (+)

productoria :: (Num a, Eq a) => Natural -> Natural -> (Natural -> a) -> a
productoria = operatoria (*)

gauss :: Natural -> Natural
gauss n = sumatoria 0 n id

{-
  Ejercicios: otras sumatorias

  Implementar y dar el tipo de las siguientes funciones simil Ejercicio 5
  Práctica 2.
-}

{-
  01 - f1(n) = ∑ i=0 -> n: 2i, n ∈ N_0.
-}

f1 :: Natural -> Natural
f1 n = sumatoria 0 n (\i -> 2 * i)

{-
  02 - f2(n, q) = ∑ i=1 -> n: q^i, n ∈ N ∧ q ∈ R.
-}

f2 :: (Num a, Eq a) => Natural -> a -> a
f2 0 _ = 0
f2 n q = sumatoria 1 n (\i -> q ^ i)

{-
  03 - f3(n, q) = ∑ i=1 -> 2n: q^i, n ∈ N_0 ∧ q ∈ R.
-}

f3 :: (Num a, Eq a) => Natural -> a -> a
f3 0 _ = 0
f3 n q = sumatoria 1 (2 * n) (\i -> q ^ i)

{-
  04 - f4(n, q) = ∑ i=n -> 2n: q^i, n ∈ N_0 ∧ q ∈ R.
-}

f4 :: (Num a, Eq a) => Natural -> a -> a
f4 0 _ = 0
f4 n q = sumatoria n (2 * n) (\i -> q ^ i)

{- ----------------------------------- o ----------------------------------- -}

{-
  Implementar una función eAprox :: Integer -> Float que aproxime el valor del
  número e a partir de la siguiente sumatoria:

  ê(n) = ∑ i=0 -> n: 1/i!

  Definir la constante e :: Float como la aproximación de e a partir de los
  primeros 10 términos de la serie anterior.

  ¡Atención! A veces ciertas funciones esperan un Float y nosotros tenemos un
  Int.
  Para estos casos podemos utilizar la función fromIntegral :: Int -> Float.
-}

factorial :: Natural -> Natural
factorial 0 = 1
factorial n = productoria 1 n id

eAprox :: Natural -> Float
eAprox n = sumatoria 0 n (\i -> 1 / fact i)
  where
    fact = fromIntegral . factorial :: Natural -> Float

e :: Float
e = eAprox 10

{- ----------------------------------- o ----------------------------------- -}

{-
  Ejercicios: sumatorias dobles
-}

{-
  01 - Implementar la siguiente función:
  f(n,m) = ∑ i=1 -> n: ∑ j=1 -> m: i^j
-}

f5 :: Natural -> Natural -> Natural
f5 0 _ = 0
f5 _ 0 = 0
f5 n m = sumatoria 1 n (\i -> sumatoria 1 m (\j -> i ^ j))

{-
  02 - Implementar una función sumaPotencias q n m que sume todas las potencias
  de la forma q^(a+b) con 1<=a<=n y 1<=b<=m.
-}

sumaPotencias :: (Num a, Eq a) => a -> Natural -> Natural -> a
sumaPotencias 0 _ _ = 0
sumaPotencias _ 0 _ = 0
sumaPotencias _ _ 0 = 0
sumaPotencias q n m = sumatoria 1 n (\a -> sumatoria 1 m (\b -> q ^ (a + b)))

{-
  03 - Implementar una función sumaRacionales n m que sume todos los números
  racionales de la forma p/q con 1<=p<=n y 1<=q<=m.
-}

sumaRacionales :: (Fractional a, Eq a) => Natural -> Natural -> a
sumaRacionales 0 _ = 0
sumaRacionales _ 0 = 0
sumaRacionales n m = sumatoria 1 n (\p -> sumatoria 1 m (\q -> float p / float q))
  where
    float = fromIntegral :: (Fractional a, Eq a) => Natural -> a

{-
  04 - Implementar la siguiente función:
  g1(i,n) = ∑ j=i -> n: i^j
-}

g1 :: Natural -> Natural -> Natural
g1 i n = sumatoria i n (\j -> i ^ j)

{-
  05 - Implementar la siguiente función:
  g2(n) = ∑ i=1 -> n: ∑ j=i -> n: i^j
-}

g2 :: Natural -> Natural
g2 0 = 0
g2 n = sumatoria 1 n (\i -> sumatoria i n (\j -> i ^ j))

{-
  06 - Implementar la siguiente función:
  g3(n) = ∑ i=1 (i es par) -> n: 2^i
-}

g3 :: Natural -> Natural
g3 0 = 0
g3 n = sumatoria 1 n (\i -> if esPar i then 2 ^ i else 0)

{-
  07 - Implementar una función que dado un n, sume todos los números naturales
  menores o iguales que n que tengan todos los dígitos iguales.
-}

g4 :: Natural -> Natural
g4 0 = 0
g4 n = sumatoria 1 n (\i -> if digitosIguales i then i else 0)