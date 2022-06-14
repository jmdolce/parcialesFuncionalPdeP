import Text.Show.Functions

-- CASOS DE PRUEBA

autoA :: Auto
autoA = Auto "rojo"  12 20
autoB :: Auto
autoB = Auto "verde" 11 15
autoC :: Auto
autoC = Auto  "azul" 23 26
autoD :: Auto
autoD = Auto  "amarillo" 23 400

carrera1 :: Carrera
carrera1 = [autoA,autoB,autoD,autoC]

-- PUNTO 1

data Auto = Auto {
    color     :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show,Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (<10) (distanciaEntreAutos auto1 auto2) && esDistinto auto1 auto2

distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1 auto2 = abs (distancia auto1 - distancia auto2)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = vaGanandoATodos auto carrera && (not . any (estaCerca auto) $ carrera)

vaGanandoATodos :: Auto -> Carrera -> Bool
vaGanandoATodos auto carrera = all (vaGanando auto) . autosDistintos auto $ carrera

vaGanando :: Auto -> Auto -> Bool
vaGanando auto1 auto2 = distancia auto1 > distancia auto2

puesto :: Auto -> Carrera -> Int
puesto auto carrera = 1 + (cantidadAutosPorDelante auto carrera)

cantidadAutosPorDelante :: Auto -> Carrera -> Int
cantidadAutosPorDelante auto carrera =length . filter (not . vaGanando auto) . autosDistintos auto $ carrera

autosDistintos :: Auto -> Carrera -> Carrera
autosDistintos auto carrera = filter (esDistinto auto) carrera

esDistinto :: Auto -> Auto -> Bool
esDistinto auto1 auto2 = auto1 /= auto2


-- PUNTO 2

correPorTiempo :: Int -> Auto -> Auto
correPorTiempo tiempo auto = cambiarDistancia tiempo auto 

cambiarDistancia :: Int -> Auto -> Auto
cambiarDistancia valor auto = auto {distancia = distancia auto + (valor * velocidad auto)}

alterarVelocidad :: (Int -> Int) -> Auto -> Auto
alterarVelocidad modificador auto = cambiarVelocidad (modificador . velocidad $ auto) auto

cambiarVelocidad :: Int -> Auto -> Auto
cambiarVelocidad valor auto = auto{velocidad = valor}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad velocidadIndicada auto = alterarVelocidad (max 0 . subtract velocidadIndicada) auto


-- PUNTO 3

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto auto carrera = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) carrera

miguelitos cantidadVelocidad auto carrera = afectarALosQueCumplen (vaGanando auto) (bajarVelocidad cantidadVelocidad) carrera

jetpack tiempo auto =cambiarVelocidad (velocidad auto) . correPorTiempo tiempo . cambiarVelocidad (velocidad auto *2) $ auto


-- PUNTO 4

correnTodos :: Int -> Carrera ->  Carrera
correnTodos tiempo carrera  = map (correPorTiempo tiempo) carrera 

usaPowerUp powerUp colorAuto carrera = powerUp (buscarAuto colorAuto) carrera

buscarAuto colorAuto carrera = filter (==colorAuto) carrera

{-simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simlarCarrera carrera eventos = map (eventos) carrera
generarTuplas auto = 
-}