data Jugador = Jugador{
    nombre        :: String,
    edad          :: Int,
    promedioDeGol :: Float,
    habilidad     :: Int,
    cansancio     :: Float
} deriving (Show)


type Equipo = (String,Char,[Jugador])

nombreEquipo (nombreEquipo, _, _) = nombreEquipo
grupo (_, grupo, _)               = grupo
jugadores (_,_,jugador)         = jugador


martin = Jugador "Martin" 26 0.0 50 35.0
juan   = Jugador "Juancho" 30 0.2 50 40.0
maxi   = Jugador "Maxi Lopez" 27 0.4 68 30.0

jonathan = Jugador "Chueco" 20 1.5 80 99.0
lean     = Jugador "Hacha" 23 0.01 50 35.0
brian    = Jugador "Panadero" 21 5 80 15.0

garcia = Jugador "Sargento" 30 1 80 13.0
messi  = Jugador "Pulga" 26 10 99 43.0
aguero = Jugador "Aguero" 24 5 90 5.0

equipo1       = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre  = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])


-- PUNTO 1

figurasEquipo :: Equipo -> [Jugador]
figurasEquipo equipo = filter esFigura (jugadores equipo)

esFigura :: Jugador -> Bool
esFigura jugador = promedioDeGol jugador > 0 && habilidad jugador > 75


-- PUNTO 2

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

tieneFarandulero :: Equipo -> Bool
tieneFarandulero equipo = any esFarandulero (jugadores equipo)

esFarandulero :: Jugador -> Bool
esFarandulero jugador = elem (nombre jugador) jugadoresFaranduleros


-- PUNTO 3

figuritasDificiles :: [Equipo] -> Char -> [Jugador]
figuritasDificiles equipos grupo = filter (esDificil) .concat . map jugadores . filter (esDelGrupo grupo) $ equipos

esDelGrupo :: Char -> Equipo -> Bool
esDelGrupo letraGrupo equipo  = grupo equipo == letraGrupo

esDificil :: Jugador -> Bool
esDificil jugador = esFigura jugador && esJoven jugador && (not . esFarandulero) jugador

esJoven :: Jugador -> Bool
esJoven jugador = edad jugador < 27



-- PUNTO 4

jugarPartido (nombreEquipo,grupo,jugadores) = (nombreEquipo,grupo, map (cansar) jugadores)

cansar jugador 
    | esDificil jugador = jugador {cansancio = 50}
    | esJoven jugador   = setCansancio (+) jugador (cansancio jugador * 0.1)
    | esFigura jugador  = setCansancio (+) jugador 20
    | otherwise         = setCansancio (*) jugador 2


setCansancio operacion jugador valor = jugador {cansancio = operacion (cansancio jugador ) valor}

-- PUNTO 5

quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter (criterio x)) xs 


ganadorDelPartido :: Equipo -> Equipo -> Equipo
ganadorDelPartido equipo1 equipo2 
    | promedioEquipo1 equipo1 > promedioEquipo2 equipo2 = jugarPartido equipo1
    | otherwise                                         = jugarPartido equipo2
    
promedioEquipo1 :: Equipo -> Float    
promedioEquipo1 equipo1 = sumaPromedioDeGol . take (11) . quickSort menosCansado . jugadores  $ equipo1

promedioEquipo2 :: Equipo -> Float
promedioEquipo2 equipo2 = sumaPromedioDeGol . take (11) . quickSort menosCansado . jugadores  $ equipo2

menosCansado :: Jugador -> Jugador -> Bool
menosCansado jugador1 jugador2 = cansancio jugador1 < cansancio jugador2

sumaPromedioDeGol :: [Jugador] -> Float
sumaPromedioDeGol jugadores =sum . map (promedioDeGol) $ jugadores


-- PUNTO 6

ganadorTorneo :: [Equipo] -> Equipo
ganadorTorneo equipos = foldl (ganadorDelPartido) (head equipos) (tail equipos) 


-- PUNTO 7

--elGroso :: [Equipo] -> Jugador
elGroso equipos = head . filter (esFigura) . jugadores . ganadorTorneo $ equipos
