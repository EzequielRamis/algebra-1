{- Primer trabajo práctico - La conjetura de Goldbach -}

{-
  01 - Escribir la función:

  satisfaceGoldbach :: Integer -> Bool

  que recibe un número natural n y devuelve True si y solo sí el n es par,
  mayor que 2 y suma de dos números primos o False en caso contrario.
-}

esParMayorQue2 :: Integral a => a -> Bool
esParMayorQue2 n = esPar n && n > 2

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n = esParMayorQue2 n && esSumaDeDosPrimos n

{-
  02 - Escribir la función:

  verificarConjeturaHasta :: Integer -> Bool

  que recibe un número natural n par mayor que 2 y devuelve True si y solo sí
  la conjetura es cierta para todos los naturales pares mayores que 2 y menores
  o iguales que n o False en caso contrario.
-}

verificarConjeturaDesde :: Integer -> Integer -> Bool
verificarConjeturaDesde i n
  -- Siendo i, q proposiciones (q genérico), e i siendo "el número actual es
  -- par, mayor que 2 y menor o igual a n" en este caso Falsa, la operación
  -- i => q es Verdadera por default
  | i > n = True
  -- Se evalúa i y el siguiente par
  | otherwise = satisfaceGoldbach i && verificarConjeturaDesde (i + 2) n

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n = esParMayorQue2 n && verificarConjeturaDesde 4 n

{-
  03 - Escribir la función:

  descomposicionEnPrimos :: Integer -> (Integer,Integer)

  que recibe un número natural n par mayor que 2 y devuelve un par ordenado
  (a,b) de números primos tales que a + b == n.
-}

descomposicionEnPrimosDesde :: Integer -> Integer -> Integer -> (Integer, Integer)
-- Sea i1, i2 índices de los primos p1, p2 respectivamente, y n el número a
-- evaluar
descomposicionEnPrimosDesde i1 i2 n
  -- Aunque se asume n par mayor que 2, agrego un caso para n diferente
  | p1 > n && p2 > n = (0, 0)
  -- Si el primer primo supera a n, reinicia su índice a 1 y se aumenta el
  -- segundo
  | p1 > n = descomposicionEnPrimosDesde 1 (i2 + 1) n
  -- Si se encuentra una suma de primos, devuelvo el par correspondiente
  | p1 + p2 == n = (p1, p2)
  -- Si no se encuentra su suma, evalúo con un siguiente primo
  | otherwise = descomposicionEnPrimosDesde (i1 + 1) i2 n
  where
    p1 = nEsimoPrimo i1
    p2 = nEsimoPrimo i2

descomposicionEnPrimos :: Integer -> (Integer, Integer)
-- Empieza desde los primos 2 y 2
descomposicionEnPrimos = descomposicionEnPrimosDesde 1 1

{-
  04 - Escribir la función:

  numeroDeDescomposiciones :: Integer -> Integer

  que recibe un número natural n par mayor que 2 y devuelve la cantidad de
  pares ordenados (a,b) de números primos tales que a + b == n
-}

numeroDeDescomposicionesDesde :: Integer -> Integer -> Integer -> Integer
-- Sea i1, i2 índices de los primos p1, p2 respectivamente, y n el número a
-- evaluar
numeroDeDescomposicionesDesde i1 i2 n
  -- Si ambos primos son mayores a n, se termina el conteo
  | p1 > n && p2 > n = 0
  -- Si el primer primo supera a n, reinicia su índice a 1 y se aumenta el
  -- segundo
  | p1 > n = numeroDeDescomposicionesDesde 1 (i2 + 1) n
  -- Si se encuentra su suma, la cuento y sigo iterando
  | p1 + p2 == n = 1 + numeroDeDescomposicionesDesde (i1 + 1) i2 n
  -- Si no se encuentra su suma, no cuento nada y sigo iterando
  | otherwise = numeroDeDescomposicionesDesde (i1 + 1) i2 n
  where
    p1 = nEsimoPrimo i1
    p2 = nEsimoPrimo i2

numeroDeDescomposiciones :: Integer -> Integer
-- Empieza desde los primos 2 y 2
numeroDeDescomposiciones = numeroDeDescomposicionesDesde 1 1

{-
  Funciones auxiliares
-}

esMultiploDe :: Integral n => n -> n -> Bool
esMultiploDe m n = mod m n == 0

esPar :: Integral n => n -> Bool
esPar n = n `esMultiploDe` 2

menorDivisorDesde :: Integral n => n -> n -> n
-- "Salvo" la indeterminación (exception en este caso)
menorDivisorDesde 0 _ = 0
menorDivisorDesde i n
  | n `esMultiploDe` i = i
  | otherwise = menorDivisorDesde (i + 1) n

menorDivisor :: Integral n => n -> n
menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde 2 n

esPrimo :: Integral n => n -> Bool
-- Todo primo es mayor a uno
esPrimo 1 = False
esPrimo n = menorDivisor n == n

nEsimoPrimoDesde :: Integral n => n -> n -> n -> n
-- Sea n, i índices de números primos y p un número Natural
nEsimoPrimoDesde n i p
  -- Si p es un primo y su índice coincide con el pedido,
  -- devuelve p
  | esPrimo p && i == n = p
  -- Si p es un primo pero su índice no coincide con el pedido,
  -- descarto el i actual y se busca el siguiente primo
  | esPrimo p = nEsimoPrimoDesde n (i + 1) (p + 1)
  -- Si p no es un primo, se busca el siguiente primo
  | otherwise = nEsimoPrimoDesde n i (p + 1)

nEsimoPrimo :: Integral n => n -> n
nEsimoPrimo n = nEsimoPrimoDesde n 1 2 -- El 1º número primo es 2

esSumaDeDosPrimosDesde :: Integral n => n -> n -> n -> Bool
-- Sea i1, i2 índices de los primos p1, p2 respectivamente, y n el número a
-- evaluar
esSumaDeDosPrimosDesde i1 i2 n
  -- Si ambos primos son mayores a n, se termina la recursión, ya que la suma
  -- no puede ser mayor a n
  | p1 > n && p2 > n = False
  -- Si el primer primo supera a n, reinicia su índice a 1 y se aumenta el
  -- segundo
  | p1 > n = esSumaDeDosPrimosDesde 1 (i2 + 1) n
  -- Si ambos son menores a n, se evalúa su suma y la del siguiente índice i1
  | otherwise = p1 + p2 == n || esSumaDeDosPrimosDesde (i1 + 1) i2 n
  where
    p1 = nEsimoPrimo i1
    p2 = nEsimoPrimo i2

esSumaDeDosPrimos :: Integral n => n -> Bool
-- Empieza desde los primos 2 y 2
esSumaDeDosPrimos = esSumaDeDosPrimosDesde 1 1