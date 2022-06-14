import Text.Show.Functions

-- PUNTO 1

data Persona = Persona{
    habilidades :: [Habilidad],
    esBueno     :: Bool
} deriving (Show)

type Habilidad = String

data PowerRanger = PowerRanger{
    color       :: String,
    habilidadesPR :: [Habilidad],
    nivelPelea  :: Int
} deriving (Show)


-- PUNTO 2

convertirEnPower :: Persona -> String -> PowerRanger
convertirEnPower persona colorNuevo = PowerRanger {color = colorNuevo, habilidadesPR = potenciarHabilidades persona, nivelPelea = cantidadLetras persona}

cantidadLetras :: Persona -> Int
cantidadLetras persona =  sum . map  length . habilidades $ persona

potenciarHabilidades :: Persona -> [Habilidad]
potenciarHabilidades persona = map (agregarSuper) (habilidades persona)

agregarSuper :: Habilidad -> Habilidad
agregarSuper habilidad = "super" ++ habilidad


-- PUNTO 3
{-

jason = Persona ["a"] True
skull = Persona ["b"] False
kimberly = Persona ["c"] True
bulk = Persona ["d"] False
 
formarEquipoRanger listaColores listaPersonas = convertir listaColores (filter (buenos) listaPersonas)

buenos :: Persona -> Bool
buenos persona = esBueno persona

-}

-- PUNTO 4.A

findOrElse condicion valor lista  = cumple condicion lista valor

cumple condicion [] valor = valor
cumple condicion (x:xs) valor
    | condicion x = x
    | otherwise   = cumple condicion xs valor

-- PUNTO 4.B

rangerLider :: [PowerRanger] -> PowerRanger
rangerLider equipoRangers 
    | (>0) . length . filtrarPorRojo $ equipoRangers = head . filtrarPorRojo $ equipoRangers
    | otherwise                                      = head equipoRangers

filtrarPorRojo :: [PowerRanger] -> [PowerRanger]
filtrarPorRojo equipoRangers = filter (colorRojo) equipoRangers

colorRojo :: PowerRanger -> Bool
colorRojo ranger = (=="rojo") . color $ ranger

-- PUNTO 5.B

rangerMasPoderoso :: [PowerRanger] -> PowerRanger
rangerMasPoderoso equipoRangers = foldl (masPoder) (head equipoRangers) (tail equipoRangers)

masPoder :: PowerRanger -> PowerRanger -> PowerRanger
masPoder ranger1 ranger2 
    | nivelPelea ranger1 > nivelPelea ranger2 = ranger1
    | otherwise                               = ranger2


-- PUNTO 6

rangerHabilidoso :: PowerRanger -> Bool
rangerHabilidoso ranger = (>5) . length . habilidadesPR $ ranger 

-- PUNTO 7

alfa5 :: PowerRanger
alfa5 = PowerRanger "Metalico" ["repararCosas",infinitosAy] 0

rojo = PowerRanger "Rojo" ["a"] 10
verde = PowerRanger "Verde" ["a","b"] 20

infinitosAy = "ay"  ++ infinitosAy

-- PUNTO 7.B

-- La funcion rangerHabilidoso con alfa5 funciona y termina 