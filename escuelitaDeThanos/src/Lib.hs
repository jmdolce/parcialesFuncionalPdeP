import Text.Show.Functions

-- PUNTO 1

data Personaje = Personaje{
    edad        :: Int,
    energia     :: Int,
    habilidades :: [String],
    nombre      :: String,
    planeta     :: String
} deriving (Show)

data Guantelete = Guantelete{
    material :: String,
    gemas    :: [Gema]
} deriving (Show)

type Universo = [Personaje]

type Gema = Personaje -> Personaje

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo
    | material guantelete == "uru" && guanteleteCompleto guantelete = reducirUniverso universo
    | otherwise                                                     = universo

guanteleteCompleto guantelete = length (gemas guantelete) == 6

reducirUniverso universo = take (div (length universo) 2) universo


-- PUNTO 2

-- Casos de prueba
juan = Personaje 120 123 ["una","dos"] "juan" "tierra"
pablo = Personaje 56 324 [] "pablo" "neptuno"
pedro = Personaje 340 83 ["tres", "cuatro"] "pedro" "saturno"

uni :: Universo
uni = [juan,pablo,pedro]


universoAptoPendex :: Universo -> Bool
universoAptoPendex  = any (<45) . map edad

energiaTotal :: Universo -> Int
energiaTotal universo =sum . map (energia) . filter (masDeUnaHabilidad) $ universo

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad personaje = (>1) . length . habilidades $ personaje 


-- PUNTO 3

mente :: Int -> Gema
mente valor personaje  = cambiarEnergia valor personaje 

cambiarEnergia :: Int -> Gema
cambiarEnergia valor personaje = personaje{energia = energia personaje - valor}

alma ::  String -> Gema
alma habilidad personaje  = cambiarEnergia (10) . eliminarHabilidad habilidad $ personaje

eliminarHabilidad :: String -> Gema
eliminarHabilidad  habilidadParticular personaje 
    | elem habilidadParticular (habilidades personaje) = personaje {habilidades = filter (/=habilidadParticular) (habilidades personaje)}
    | otherwise                                        = personaje

espacio :: String -> Gema
espacio planeta personaje = cambiarEnergia (20) . cambiarPlaneta planeta $ personaje

cambiarPlaneta :: String -> Gema
cambiarPlaneta planetaNuevo personaje = personaje {planeta = planetaNuevo}

poder :: Gema
poder personaje = cambiarEnergia 0 . cantidadHabilidades  $ personaje

cantidadHabilidades :: Gema
cantidadHabilidades personaje 
    | (<2) . length . habilidades $ personaje = personaje { habilidades = []}
    | otherwise                               = personaje

tiempo :: Gema
tiempo personaje = cambiarEnergia(50) . reducirEdad $ personaje

reducirEdad :: Gema
reducirEdad personaje 
    | divisionEdad personaje < 18 = personaje {edad=18}
    | otherwise                   = personaje{edad = divisionEdad personaje}

divisionEdad :: Personaje -> Int
divisionEdad personaje = div (edad personaje) 2

gemaLoca :: Gema -> Gema
gemaLoca gema personaje = gema . gema $ personaje


-- PUNTO 4

--guantelete = Guantelete "goma" [tiempo,alma,gemaLoca]


-- PUNTO 5

utilizar gemas personaje = foldl (personaje) gemas

-- PUNTO 6

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = laMasPoderosa personaje (gemas guantelete)

laMasPoderosa :: Personaje -> [Gema] -> Gema
laMasPoderosa _ [gema] = gema
laMasPoderosa personaje (gema1:gema2:gemas)
    | (energia . gema1) personaje > (energia . gema2) personaje = laMasPoderosa personaje (gema1:gemas) 
    | otherwise                                                 = laMasPoderosa personaje (gema2:gemas) 