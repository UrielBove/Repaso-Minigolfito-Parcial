module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Int = Number

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro 10  (2 * precisionJugador habilidad) 0

madera :: Palo
madera hab = UnTiro 100 (precisionJugador hab / 2) 5

hierros :: Number -> Palo
hierros n hab = UnTiro (fuerzaJugador hab * n) (precisionJugador hab / n) (max 0 (n - 3))

palos :: [Palo]
palos = [putter,madera]++ map hierros [1..10]


--2

golpe :: Palo -> Jugador -> Tiro
golpe palo = palo . habilidad

--3
data Obstaculo = UnObstaculo{
    superaObstaculo :: SuperaObstaculo,
    efectoObstaculo :: EfectoObstaculo
}deriving(Show,Eq)

type SuperaObstaculo = Tiro -> Bool
type EfectoObstaculo = Tiro -> Tiro

intentarSuperar :: Obstaculo -> Tiro -> Tiro
intentarSuperar obstaculo tiro 
    |superaObstaculo obstaculo tiro = efectoObstaculo obstaculo tiro
    |otherwise = tiroDetenido


tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = (modifVelocidad (2 * velocidad tiro) . modifPrecision 100 . modifAltura 0) tiro

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = (>90) (precision tiro) && (==0) (altura tiro)

laguna :: Number -> Obstaculo
laguna largoLag = UnObstaculo superaLaguna (efectoLaguna largoLag)

superaLaguna :: SuperaObstaculo
superaLaguna tiro = (>80) (velocidad tiro) && between 1 5 (altura tiro)

efectoLaguna :: Number -> EfectoObstaculo
efectoLaguna largoLag tiro = modifAltura (altura tiro / largoLag ) tiro

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: SuperaObstaculo
superaHoyo tiro = between 5 20 (velocidad tiro) && (==0) (altura tiro) && (>95) (precision tiro)

efectoHoyo :: EfectoObstaculo
efectoHoyo = modifAltura 0 . modifPrecision 0 . modifVelocidad 0

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

modifAltura :: Number -> Tiro -> Tiro
modifAltura n tiro = tiro{altura = n}

modifPrecision :: Number -> Tiro -> Tiro
modifPrecision n tiro = tiro{precision= n}

modifVelocidad :: Number -> Tiro -> Tiro
modifVelocidad n tiro = tiro{velocidad = n}

--4a

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jug obs = filter (leSirveParaSuperar jug obs) palos 

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jug obs palo = superaObstaculo obs (golpe palo jug)

--4b

cuantosObsConsecutivos :: Tiro -> [Obstaculo] -> Number
cuantosObsConsecutivos _ [] = 0
cuantosObsConsecutivos tiro (o:os) | superaObstaculo o tiro  = 1 + cuantosObsConsecutivos (efectoObstaculo o tiro) os
    |otherwise = 0

tiro1 :: Tiro
tiro1 = UnTiro 10 95 0

obstaculos1 :: [Obstaculo]
obstaculos1 = [tunelConRampita,tunelConRampita,hoyo]
--c
paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jug obs = foldl1 (maximoEntreDosPalos jug obs) palos 

--maximoSegun f = foldl1 (mayorSegun f)
--mayorSegun f a b
--  | f a > f b = a
--  | otherwise = b

paloMasUtil2 :: Jugador -> [Obstaculo] -> Palo
paloMasUtil2 jug obs = maximoSegun (flip cuantosObsConsecutivos obs . flip golpe jug)palos

maximoEntreDosPalos :: Jugador -> [Obstaculo] -> Palo -> Palo -> Palo
maximoEntreDosPalos jug obs p1 p2  
    |cuantosObsConsecutivos (golpe p1 jug) obs > cuantosObsConsecutivos (golpe p2 jug) obs = p1
    |otherwise = p2

--5

jugadorDelTorneo :: (a, b) -> a
jugadorDelTorneo = fst
puntosDelTorneo :: (a, b) -> b
puntosDelTorneo = snd


padresQuePierden :: [(Jugador, Number)] -> [String]
padresQuePierden puntosDelTorneo= (map (padre . jugadorDelTorneo) .filter (not . ganoNinio puntosDelTorneo))puntosDelTorneo

ganoNinio :: [(Jugador, Number)]  -> (Jugador, Number) -> Bool
ganoNinio puntosDelTorneo puntosDeUnJug = (all (<puntosGanados puntosDeUnJug) . puntosGanados) . filter(/= puntosDeUnJug)puntosDelTorneo
