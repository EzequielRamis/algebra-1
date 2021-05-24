{- Clase 7 - Conjuntos -}

module Clases.Clase07 where

import Clases.Clase02 (crearPar)
import Clases.Clase06 (eliminarRepetidosAlFinal, ordenar)

type Set a = [a]

vacio :: Set a
vacio = [] -- ∅

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False -- ∀a ∈ A: a ∉ ∅
pertenece x (a : as) = x == a || pertenece x as

agregar :: a -> Set a -> Set a
agregar = (:)

normalizar :: Ord a => Set a -> Set a
normalizar a = ordenar $ eliminarRepetidosAlFinal a

{-
  Más operaciones sobre Conjuntos
-}

{-
  01 - incluido :: Set Int -> Set Int -> Bool que determina si el primer
  conjunto está incluido en el segundo.
-}

incluido :: Set Int -> Set Int -> Bool
incluido [] _ = True -- ∀A: ∅ ⊆ A
incluido (a : as) b = pertenece a b && incluido as b

{-
  02 - iguales :: Set Int -> Set Int -> Bool que determina si dos conjuntos son
  iguales.
-}

iguales :: Set Int -> Set Int -> Bool
iguales a b = normalizar a == normalizar b

{-
  Tarea: Parte I
-}

{-
  01 - union :: Set Int -> Set Int -> Set Int que dado dos conjuntos, devuelve
  la unión entre ellos.
-}

union :: Set Int -> Set Int -> Set Int
union a b = normalizar $ union' a b

union' :: Set Int -> Set Int -> Set Int
union' a b = a ++ b

{-
  02 - interseccion :: Set Int -> Set Int -> Set Int que dado dos conjuntos,
  devuelve la interesección entre ellos.
-}

interseccion :: Integral n => Set n -> Set n -> Set n
interseccion a b = normalizar $ interseccion' a b

interseccion' :: Integral n => Set n -> Set n -> Set n
interseccion' [] _ = [] -- ∀A: A ⋂ ∅ = ∅
interseccion' (a : as) b
  | pertenece a b = a : interseccion' as b
  | otherwise = interseccion' as b

{-
  03 - diferencia :: Set Int -> Set Int -> Set Int que dado los conjuntos
  A y B, devuelve A - B.
-}

diferencia :: Set Int -> Set Int -> Set Int
diferencia a b = normalizar $ diferencia' a b

diferencia' :: Set Int -> Set Int -> Set Int
diferencia' [] _ = [] -- ∀B: ∅ - B = ∅.
diferencia' (a : as) b
  | pertenece a b = diferencia' as b
  | otherwise = a : diferencia' as b

{-
  04 - diferenciaSimetrica :: Set Int -> Set Int -> Set Int que dado los
  conjuntos A y B, devuelve la diferencia simétrica, es decir, A △ B.
-}

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = normalizar $ diferenciaSimetrica' a b

diferenciaSimetrica' :: Set Int -> Set Int -> Set Int
diferenciaSimetrica' a b = diferencia' a b `union'` diferencia' b a

{-
  Partes de un conjunto
-}

{-
  01 - partes :: Set Int -> Set (Set Int) que genere el conjunto de partes de
  un conjunto dado.
  Por ejemplo, todos los subconjuntos del conjunto
  [1,2,3] ~> [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]].
-}

partes :: Set Int -> Set (Set Int)
partes a = ordenar $ partes' a

partes' :: Set Int -> Set (Set Int)
partes' [] = [[]]
partes' (a : as) = partes' as ++ map (agregar a) (partes' as)

{-
  Tarea: Parte II
-}

{-
  01 - partesN :: Int -> Set (Set Int) que genere los subconjuntos del conjunto
  {1,2,3,...,n}. Ejemplo: 2 ~> [[],[1],[2],[1,2]]
-}

partesN :: Int -> Set (Set Int)
partesN n = partes [1 .. n]

{-
  02 - productoCartesiano :: Set Int -> Set Int -> Set (Int,Int) que dados dos
  conjuntos genere todos los pares posibles (como pares de dos elementos)
  tomando el primer elemento del primer conjunto y el segundo elemento del
  segundo conjunto.
  Ejemplo: [1,2,3] X [3,4] ~> [(1,3),(2,3),(3,3),(1,4),(2,4),(3,4)]
-}

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano [] _ = []
productoCartesiano _ [] = []
productoCartesiano a (b : bs) = map (`crearPar` b) a ++ productoCartesiano a bs