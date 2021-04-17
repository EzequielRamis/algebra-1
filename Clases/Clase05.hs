{- Clase 5 - Recursión con funciones auxiliares -}

module Clases.Clase05 where

import Clases.Clase01 (esMultiploDe)
import Clases.Clase02 (esPar)
import Clases.Clase04 (factorial, operatoria, sumatoria)
import GHC.Natural (Natural)

{-
  ¿Una fácil?.. o no tanto

  sumaDivisores :: Int -> Int que calcule la suma de los divisores un entero
  positivo.

  ¿Qué sucede si construimos una funcion más general que nos facilita el
  trabajo?

  sumaDivisoresHasta :: Int -> Int -> Int que devuelve la suma de los divisores
  de un número hasta cierto punto.
-}

{-
  01 - Implementar una función sumaDivisoresHasta :: Int -> Int -> Int
-}

-- No hace falta

{-
  02 - Implementar la función sumaDivisores en función de la anterior.
-}

sumaDivisores :: Natural -> Natural
sumaDivisores n = sumatoria 1 n (\i -> if esMultiploDe n i then i else 0)

{-
  Un entero p>1 es primo si no existe un natural k tal que 1<k<p y k divide a
  p.
-}

{-
  03 - Implementar menorDivisor :: Int -> Int que calcule el menor divisor
  (mayor que 1) de un natural n.
-}

menorDivisorDesde :: Natural -> Natural -> Natural
menorDivisorDesde _ 0 = 0
menorDivisorDesde _ 1 = 1
menorDivisorDesde i n
  | esMultiploDe n i = i
  | otherwise = menorDivisorDesde (i + 1) n

menorDivisor :: Natural -> Natural
menorDivisor = menorDivisorDesde 2

{-
  04 - Implementar la función esPrimo :: Int -> Bool.
-}

esPrimo :: Natural -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

{-
  05 - Implementar la función nEsimoPrimo :: Int -> Int que devuelve el n-esimo
  primo (n>=1, el primer primo es el 2, el segundo es el 3, el tercero es el 5,
  etc.)
-}

nEsimoPrimoDesde :: Natural -> Natural -> Natural -> Natural
nEsimoPrimoDesde n i p
  | esPrimo p = if i == n then p else nEsimoPrimoDesde n (i + 1) (p + 1)
  | otherwise = nEsimoPrimoDesde n i (p + 1)

nEsimoPrimo :: Natural -> Natural
nEsimoPrimo n = nEsimoPrimoDesde n 1 2

{-
  06 - Implementar menorFactDesde :: Int -> Int que dado m>=1 encuentra el
  mínimo n>=m tal que n=k! para algún k.
-}

menorFact :: Natural -> Natural -> Natural
menorFact m k
  | n < m = menorFact m (k + 1)
  | otherwise = n
  where
    n = factorial k

menorFactDesde :: Natural -> Natural
menorFactDesde n = menorFact n 1

{-
  07 - Implementar mayorFactHasta :: Int -> Int que dado m>=1 encuentra el
  máximo n<=m tal que n=k! para algún k.
-}

mayorFact :: Natural -> Natural -> Natural
mayorFact m k
  | n > m = factorial (k - 1)
  | otherwise = mayorFact m (k + 1)
  where
    n = factorial k

mayorFactHasta :: Natural -> Natural
mayorFactHasta n = mayorFact n 1

{-
  08 - Implementar esFact :: Int -> Bool que dado n>=0 decide si existe un
  número entero k>=0 tal que n=k!
-}

esFactDesde :: Natural -> Natural -> Bool
esFactDesde k n
  | m > n = False
  | otherwise = m == n || esFactDesde (k + 1) n
  where
    m = factorial k

esFact :: Natural -> Bool
esFact = esFactDesde 1

{-
  09 - Implementar esFibonacci :: Int -> Bool que dado un número entero n>=0
  decide si n es un número de Fibonacci.
-}

fibonacci :: Natural -> Natural
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

esFiboDesde :: Natural -> Natural -> Bool
esFiboDesde k n
  | m > n = False
  | otherwise = m == n || esFiboDesde (k + 1) n
  where
    m = fibonacci k

esFibonacci :: Natural -> Bool
esFibonacci = esFiboDesde 0

{-
  10 - Implementar esSumaInicialDePrimos :: Int -> Bool que dado un número
  entero n>=0 decide si n es igual a la suma de los m primeros números primos,
  para algún m.
-}

sumaInicialDePrimos :: Natural -> Natural
sumaInicialDePrimos n = sumatoria 1 n nEsimoPrimo

esSumaInicialDePrimosDesde :: Natural -> Natural -> Bool
esSumaInicialDePrimosDesde k n
  | m > n = False
  | otherwise = m == n || esSumaInicialDePrimosDesde (k + 1) n
  where
    m = sumaInicialDePrimos k

esSumaInicialDePrimos :: Natural -> Bool
esSumaInicialDePrimos = esSumaInicialDePrimosDesde 1

{-
  11 - Implementar tomaValorMax :: Int -> Int -> Int que dado un número entero
  n_1>=1 y un n_2>=n_1 devuelve algún m entre n_1 y n_2 tal que

  sumaDivisores(m) = max{ sumaDivisores(i) | n_1<=i<=n_2 }
-}

tomaValorMaxDesde :: Natural -> Natural -> Natural -> Natural
tomaValorMaxDesde n1 n2 m
  | n1 > n2 = m
  | sumaDivisores n1 > sumaDivisores m = tomaValorMaxDesde (n1 + 1) n2 n1
  | otherwise = tomaValorMaxDesde (n1 + 1) n2 m

tomaValorMax :: Natural -> Natural -> Natural
tomaValorMax n1 n2 = tomaValorMaxDesde n1 n2 n1

{-
  12 - Implementar tomaValorMin :: Int -> Int -> Int que dado un número entero
  n_1>=1 y un n_2>=n_1 devuelve algún m entre n_1 y n_2 tal que

  sumaDivisores(m) = min{ sumaDivisores(i) | n_1<=i<=n_2 }
-}

tomaValorMinDesde :: Natural -> Natural -> Natural -> Natural
tomaValorMinDesde n1 n2 m
  | n1 > n2 = m
  | sumaDivisores n1 < sumaDivisores m = tomaValorMinDesde (n1 + 1) n2 n1
  | otherwise = tomaValorMinDesde (n1 + 1) n2 m

tomaValorMin :: Natural -> Natural -> Natural
tomaValorMin n1 n2 = tomaValorMinDesde n1 n2 n1

{-
  13 - Implementar esSumaDeDosPrimos :: Int -> Bool que, dado un número natural
  n, determine si puede escribirse como suma de dos números primos.
-}

esSumaDeDosPrimosDesde :: Natural -> Natural -> Natural -> Bool
esSumaDeDosPrimosDesde i2 i1 n
  | p1 > n && p2 > n = False
  | p1 > n = esSumaDeDosPrimosDesde (i2 + 1) 1 n
  | otherwise = p1 + p2 == n || esSumaDeDosPrimosDesde i2 (i1 + 1) n
  where
    p1 = nEsimoPrimo i1
    p2 = nEsimoPrimo i2

esSumaDeDosPrimos :: Natural -> Bool
esSumaDeDosPrimos = esSumaDeDosPrimosDesde 1 1

{-
  14 - Conjetura de Christian Goldbach, 1742: todo número par mayor que 2 puede
  escribirse como suma de dos números primos.
  Escribir una función que pruebe la conjetura hasta un cierto punto.

  goldbach :: Int -> Bool (hasta al menos 4*10^18 debería ser cierto)
-}

algun :: Natural -> Natural -> (Natural -> Bool) -> Bool
algun = operatoria (||)

goldbachDesde :: Natural -> Natural -> Bool
goldbachDesde i n = algun i n (\i -> esPar i && esSumaDeDosPrimos i)

goldbach :: Natural -> Bool
goldbach = goldbachDesde 4

{-
  15 - Los números naturales a y b forman un par de primos gemelos si b=a+2 y
  tanto a como b son primos.
  Implementar primosGem :: Int -> Int que dado n,
  devuelve la cantidad de pares de primos gemelos (a, b) que verifican b<=n.
  Por ejemplo: primosGem 5 = 1 (porque 3 y 5 es un par de primos gemelos)
  primosGem 14 = 3 (porque 3 y 5, 5 y 7, y 11 y 13 son tres pares de primos gemelos).
-}

primosGemDesde :: Natural -> Natural -> Natural
primosGemDesde b n
  | b > n = 0
  | otherwise = (if esPrimo a && esPrimo b then 1 else 0) + primosGemDesde (b + 1) n
  where
    a = b - 2

primosGem :: Natural -> Natural
primosGem = primosGemDesde 3

{-
  16 - Conjetura de los primos gemelos: Existen infinitos pares de primos
  gemelos. Implementar la función proxPrimosGem :: Int -> (Int, Int) que dado n
  devuelve el primer par de gemelos (a, b) tal que a>n.
-}

actualPrimosGem :: Natural -> (Natural, Natural)
actualPrimosGem a
  | esPrimo a && esPrimo b = (a, b)
  | otherwise = actualPrimosGem $ a + 1
  where
    b = a + 2

proxPrimosGem :: Natural -> (Natural, Natural)
proxPrimosGem n = actualPrimosGem $ n + 1

{-
  17 - Conjetura de Lothar Collatz, 1937: sea la siguiente definición:

  a_{n+1} = | a_n/2      si a_n es par
            | 3a_n + 1   si a_n es impar

  empezando a_1 con cualquier entero positivo siempre se llega a 1.
  Por ejemplo, si a_1= 13, obtenemos la siguiente secuencia:
  13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
  (9 reducciones, o sea 9 flechas).

  a. Implementar largoSecuencia :: Int -> Int que dado un n>0 devuelve la
  cantidad de reducciones desde a_1=n hasta llegar a 1. Por ejemplo,
  largoSecuencia 13 es 9.

  b. Resolver usando Haskell: ¿qué número menor a 10.000 para a_1 produce la
  secuencia de números más larga hasta llegar a 1?
  Sugerencia: usar la idea de la función del ejercicio 11.
-}

collatz :: Natural -> Natural
collatz n
  | esPar n = div n 2
  | otherwise = 3 * n + 1

largoSecuenciaDesde :: Natural -> Natural -> Natural
largoSecuenciaDesde i n
  | n == 1 = i
  | otherwise = largoSecuenciaDesde (i + 1) (collatz n)

largoSecuencia :: Natural -> Natural
largoSecuencia = largoSecuenciaDesde 0

collatzMasLargaDesde :: Natural -> Natural -> Natural -> (Natural, Natural)
collatzMasLargaDesde i n m
  | i > n = (largoMax, m)
  | largoAct > largoMax = collatzMasLargaDesde (i + 1) n i
  | otherwise = collatzMasLargaDesde (i + 1) n m
  where
    largoAct = largoSecuencia i
    largoMax = largoSecuencia m

collatzMasLarga :: Natural -> (Natural, Natural)
collatzMasLarga n = collatzMasLargaDesde 1 n 1