{- Clase 3 - Recursión -}

module Clases.Clase03 where

import Clases.Clase01 (digitoDecenas, digitoUnidades)

{-
  01 - Escribir una función para determinar si un número natural es múltiplo de
  3. No está permitido utilizar mod ni div.
-}

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n
  | n < 3 = False
  | n == 3 = True
  | otherwise = esMultiploDe3 $ n - 3

{-
  02 - Implementar la función sumaImpares :: Int -> Int que dado n ∈ N sume los
  primeros n números impares. Ej: sumaImpares 3 ~> 1+3+5 ~> 9.
-}

sumaImpares :: Int -> Int
sumaImpares 1 = 1
sumaImpares n = impar + sumaImpares (n - 1)
  where
    impar = 2 * n - 1

{-
  03 - Escribir una función medioFact que dado n ∈ N calcula
  n! = n(n−2)(n−4)···. Por ejemplo:

  medioFact 10 ~> 10∗8∗6∗4∗2 ~> 3840.
  medioFact 9 ~> 9∗7∗5∗3∗1 ~> 945.
-}

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n - 2)

{-
  04 - Escribir una función que determine la suma de dígitos de un número
  positivo. Para esta función pueden utilizar div y mod.
-}

sumaDigitos :: Int -> Int
sumaDigitos n
  | n < 10 = n
  | otherwise = digitoUnidades n + sumaDigitos (div n 10)

{-
  05 - Implementar una función que determine si todos los dígitos de un número
  son iguales.
-}

digitosIguales :: (Integral a) => a -> Bool
digitosIguales n
  | n < 10 = True
  | otherwise = digitoUnidades n == digitoDecenas n && digitosIguales (div n 10)