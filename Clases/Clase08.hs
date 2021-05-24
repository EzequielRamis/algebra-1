{- Clase 8 - Combinatoria -}

module Clases.Clase08 where

import Clases.Clase04 (factorial)
import Clases.Clase07 (Set, agregar, diferencia', normalizar, partes, union')
import GHC.Natural (Natural)

{-
  Escribir una función que dados n,k ∈ N tal que 0<=k<=n, compute el
  combinatorio C(n,k).
  Hacerlo usando la igualdad C(n,k) = C(n-1,k) + C(n-1,k-1) para 1<=k<=n-1
-}

combinatorio :: Natural -> Natural -> Natural
combinatorio _ 0 = 1
combinatorio n 1 = n
combinatorio n k | n == k = 1
combinatorio n k = combinatorio (n - 1) k + combinatorio (n - 1) (k - 1)

{-
  Implementar una función variaciones :: Set Int -> Int -> Set [Int] que dado
  un conjunto c y una longitud k genere todas las posibles listas de longitud k
  a partir de elementos de c.
  Ej: variaciones [4,7] 3
  ~> [[4,4,4],[4,4,7],[4,7,4],[4,7,7],[7,4,4],[7,4,7],[7,7,4],[7,7,7]]
-}

{-
  -variaciones [4,7] 0 = [[]]
  -variaciones [4,7] 1 = [[4],[7]]
  -variaciones [4,7] 2 = [[4,4],[4,7],[7,4],[7,7]]
-}

variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c k = agregarCasosAnteriores c $ variaciones c $ k - 1

agregarCasosAnteriores :: Set Int -> Set [Int] -> Set [Int]
agregarCasosAnteriores _ [] = []
agregarCasosAnteriores c (d : ds) = map (\x -> d ++ [x]) c ++ agregarCasosAnteriores c ds

variaciones' :: Enum a => Set a -> Int -> Set [a]
variaciones' c k = variacionesDesde c k (length c ^ k - 1)

variacionesDesde :: Enum a => Set a -> Int -> Int -> Set [a]
variacionesDesde c@(cx : _) k i
  | i == 0 = [take k padding]
  | otherwise = variacionesDesde c k (i - 1) ++ [take (k - length n) padding ++ n]
  where
    n = numeracion c i
    padding = repeat cx

numeracion :: Set a -> Int -> Set a
numeracion [] _ = []
numeracion [s] _ = []
numeracion s n | null (numeracion' s n) = [head s]
numeracion s n = numeracion' s n

numeracion' :: Set a -> Int -> Set a
numeracion' _ 0 = []
numeracion' (_ : s : _) 1 = [s]
numeracion' s n = numeracion' s (div n len) ++ [s !! mod n len]
  where
    len = length s

hex = "0123456789ABCDEF"

dec = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

bin = [0, 1]

{-
  Implementar una función insertarEn :: [Int] -> Int -> Int -> [Int] que dados
  una lista l, un número n y una posición i (contando desde 1) devuelva una
  lista en donde se insertó n en la posición i de l y los elementos siguientes
  corridos en una posición.
  Ej: insertarEn [1,2,3,4,5] 6 2 ~> [1,6,2,3,4,5]
-}

insertarEn :: [a] -> a -> Int -> [a]
insertarEn l _ 0 = l
insertarEn l n 1 = n : l
insertarEn (lx : ls) n i = lx : insertarEn ls n (i - 1)

{-
  Implementar una función permutaciones :: Set Int -> Set [Int] que dado un
  conjunto de enteros, genere todas las posibles permutaciones de los números
  del conjunto pasado por parámetro.
  Ej: permutaciones [1,2,3]
  ~> [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-}

permutaciones :: Set a -> Set [a]
permutaciones [] = [[]]
permutaciones (c : cs) = opcionesEnCadaLista (permutaciones cs) c

opcionesEnCadaLista :: Set [a] -> a -> Set [a]
opcionesEnCadaLista [] _ = []
opcionesEnCadaLista (cs : css) k = opciones cs k (length cs + 1) ++ opcionesEnCadaLista css k

opciones :: Set a -> a -> Int -> Set [a]
opciones c k 1 = [insertarEn c k 1]
opciones c k i = opciones c k (i - 1) ++ [insertarEn c k i]

{-
  Ejercicios: Implementar funciones que devuelvan
-}

{-
  01 - Todas las formas de ubicar n bolitas numeradas en k cajas.
  bolitasEnCajas :: Int -> Int -> Set [Int]
  Ej: bolitasEnCajas 2 3
  ~> [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]

  Notar que el elemento i de cada sublista representa el número de caja donde
  fue a parar la bolita i.
-}

bolitasEnCajas :: Int -> Int -> Set [Int]
bolitasEnCajas n k = variaciones [1 .. k] n

{-
  02 - Todas las formas de ubicar n bolitas numeradas en k cajas tal que la
  primera caja nunca esté vacía.
-}

bolitasEnCajas' :: Int -> Int -> Set [Int]
bolitasEnCajas' n k = map (agregar 1) $ variaciones [2 .. k] (n - 1)

{-
  03 - Todas las listas ordenadas de k números distintos tomados del conjunto
  {1,...,n}.
-}

todasLasListasOrdenadas :: Int -> Int -> Set [Int]
todasLasListasOrdenadas k n = normalizar $ todasLasListasOrdenadasDesde k n k

todasLasListasOrdenadasDesde :: Int -> Int -> Int -> Set [Int]
todasLasListasOrdenadasDesde k n 0 = []
todasLasListasOrdenadasDesde k n i = todasLasListasOrdenadasDesde k n (i - 1) ++ listasOrdenadas k i n

listasOrdenadas :: Int -> Int -> Int -> Set [Int]
listasOrdenadas k i n = filter (\x -> length x == k) $ partes [i .. n]

{-
  04 - Todas las sucesiones de los caracteres ’a’ y ’b’ de longitud n y m
  respectivamente.
-}

sucesionesAB :: Int -> Int -> Set [Char]
sucesionesAB n m = normalizar $ permutaciones ab
  where
    ab = a ++ b
    a = replicate n 'a'
    b = replicate m 'b'

{-
  05 - Todas las sucesiones de ’a’, ’b’ y ’c’ de longitud n, m y k
  respectivamente.
-}

sucesionesABC :: Int -> Int -> Int -> Set [Char]
sucesionesABC n m k = normalizar $ permutaciones abc
  where
    abc = a ++ b ++ c
    a = replicate n 'a'
    b = replicate m 'b'
    c = replicate k 'c'

{-
  06 - Implementar una función subconjuntos :: Set Int -> Int -> Set (Set Int)
  que dados un conjunto de enteros y un entero k, genera todos los subconjuntos
  de k elementos del conjunto pasado por parámetro.
  Ej: subjconjuntos [1,2,3] 2 ~> [[1,2],[2,3],[1,3]]
-}

subconjuntos :: Set Int -> Int -> Set (Set Int)
subconjuntos e k = filter (\x -> length x == k) $ partes e