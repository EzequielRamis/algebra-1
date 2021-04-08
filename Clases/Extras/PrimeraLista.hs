{- Ejercicios extra - Primera lista -}

module Clases.Extras.PrimeraLista where

{-
  01 - Escriba una función dígito :: Int -> Integer -> Integer tal que la
  expresión dígito i n calcule el i-ésimo dígito decimal del entero n,
  asumiendo que n es un entero no negativo y que i es un entero positivo.
-}

unidad :: Integral a => a -> a
unidad n = mod n 10

digito :: Int -> Integer -> Integer
digito i n = unidad $ div n $ 10 ^ (i - 1)

{-
  02 - Escriba una función sumaDeDígitos :: Integer -> Integer tal que la
  expresión sumaDeDígitos n sea la suma de los dígitos del entero n, que
  asumimos es no negativo.
-}

sumaDeDigitos :: Integer -> Integer
sumaDeDigitos n
  | n < 10 = n
  | otherwise = unidad n + sumaDeDigitos (div n 10)

{-
  03 - Escriba una función dígitoMáximo :: Integer -> Integer tal que la
  expresión dígitoMáximo n sea el dígito más grande de n, que asumimos es no
  negativo.
-}

digitoMaximo :: Integer -> Integer
digitoMaximo n
  | n < 10 = n
  | otherwise = max (unidad n) (digitoMaximo $ div n 10)

{-
  04 - Escriba una función factorial :: Integer -> Integer tal que factorial n
  sea el factorial del entero no negativo n.
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
  05 - Escriba una función e_approx :: Integer -> Double tal que para cada
  entero no negativo n el valor de e_approx n sea

  ∑ k=0 -> n = 1/k!
-}

eApprox :: Integer -> Double
eApprox 0 = 1
eApprox n = 1 / fact + eApprox (n - 1)
  where
    fact = fromInteger (factorial n) :: Double

{-
  06 - Escriba una función unos :: Integer -> Integer tal que para cada entero
  no negativo n el valor de unos n sea la cantidad de dígitos 1 en la escritura
  binaria de n.
-}

unos :: Integer -> Integer
unos 0 = 0
unos n = mod n 2 + unos (div n 2)

{-
  07 - Escriba una función pi_approx :: Integer -> Double tal que para cada
  entero positivo n el valor de pi_approx sea

  3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) + ... ± 4/(2n * (2n + 1) * (2n + 2))

  Observe que los sumandos marcados van alternando en signo — en particular, el
  signo del último sumando depende de la paridad de n.
-}

piApprox :: Integer -> Double
piApprox 0 = 3
piApprox n = piApprox (n - 1) + par * (4 / denominador)
  where
    par = (-1) ** (n' + 1)
    denominador = doble * (doble + 1) * (doble + 2)
    doble = 2 * n'
    n' = fromInteger n :: Double