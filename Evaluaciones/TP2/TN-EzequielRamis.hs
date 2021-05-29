{- Segundo trabajo práctico - El juego de Pop-it -}

type Piedras = Int

type Pila = Int

type Posicion = [Piedras]

type Jugada = (Pila, Piedras)

{-
  01 - Escribir la función

  jugar :: Posicion -> Jugada -> Posicion

  que recibe una posición p, una jugada válida j y devuelve la posición
  obtenida al realizar dicha jugada.
-}

jugar :: Posicion -> Jugada -> Posicion
jugar = jugarDesde 1

jugarDesde :: Pila -> Posicion -> Jugada -> Posicion
jugarDesde i (p : ps) j
  | estaEnPila && pilaVacia = ps
  | estaEnPila = piedrasRestantes : ps
  | otherwise = p : jugarDesde (i + 1) ps j
  where
    estaEnPila = fst j == i
    pilaVacia = piedrasRestantes == 0
    piedrasRestantes = p - snd j

{-
  02 - Escribir la función

  posiblesJugadas :: Posicion -> [Jugada]

  que recibe una posición p y devuelve el conjunto de jugadas válidas a partir
  de p.
-}

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas = posiblesJugadasDesde 1

posiblesJugadasDesde :: Pila -> Posicion -> [Jugada]
posiblesJugadasDesde _ [] = []
posiblesJugadasDesde i [p] = posiblesJugadasEnPila (i, p)
posiblesJugadasDesde i (p : ps) = posiblesJugadasEnPila (i, p) ++ posiblesJugadasDesde (i + 1) ps

posiblesJugadasEnPila :: Jugada -> [Jugada]
posiblesJugadasEnPila (i, 1) = [(i, 1)]
posiblesJugadasEnPila (i, p) = posiblesJugadasEnPila (i, p - 1) ++ [(i, p)]

{-
  03 - Escribir la función

  esPosicionGanadora :: Posicion -> Bool

  que decide si una posición p es ganadora.
-}

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora p = existeSigPosicionPerdedora p (posiblesJugadas p)

existeSigPosicionPerdedora :: Posicion -> [Jugada] -> Bool
-- Como ya no hay jugadas disponibles a ejecutar, es falsa por default
existeSigPosicionPerdedora _ [] = False
existeSigPosicionPerdedora p (j : js) = esSigPosicionPerdedora p j || existeSigPosicionPerdedora p js

esSigPosicionPerdedora :: Posicion -> Jugada -> Bool
esSigPosicionPerdedora p j = not (esPosicionGanadora (siguientePosicion p j))

siguientePosicion :: Posicion -> Jugada -> Posicion
siguientePosicion = jugar

{-
  04 - Escribir la función

  jugadaGanadora :: Posicion -> Jugada

  que recibe una posición ganadora p y devuelve una jugada que dejaría al rival
  en una posición no ganadora.
-}

jugadaGanadora :: Posicion -> Jugada
jugadaGanadora p = head (jugadasGanadoras p)

jugadasGanadoras :: Posicion -> [Jugada]
jugadasGanadoras p = jugadasGanadorasCon (posiblesJugadas p) p

jugadasGanadorasCon :: [Jugada] -> Posicion -> [Jugada]
-- Como ya no hay jugadas disponibles a ejecutar, se termina la recursión
jugadasGanadorasCon [] _ = []
jugadasGanadorasCon (j : js) p
  | esSigPosicionPerdedora p j = j : jugadasGanadorasCon js p
  | otherwise = jugadasGanadorasCon js p

{-
  05 - Escribir la función

  numeroDeJugadasGanadoras :: Posicion -> Int

  que recibe una posición p (no necesariamente ganadora) y devuelve la cantidad
  de jugadas ganadoras partiendo de p.
-}

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras p = length (jugadasGanadoras p)