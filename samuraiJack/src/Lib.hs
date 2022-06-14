import Text.Show.Functions

data Elemento = Elemento { 
    tipo    :: String,
    ataque  :: (Personaje-> Personaje),
    defensa :: (Personaje-> Personaje) 
} deriving (Show)


data Personaje = Personaje { 
    nombre       :: String,
    salud        :: Float,
    elementos    :: [Elemento],
    anioPresente :: Int 
} deriving (Show)


-- PUNTO 1

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje {salud = salud personaje / 2}

causarDanio :: Float -> Personaje -> Personaje
causarDanio danio personaje = personaje {salud = max 0 (salud personaje - danio)}


-- PUNTO 2

esMalvado :: Personaje -> Bool
esMalvado personaje = any ((=="Maldad") . tipo)  (elementos personaje)

danioQueProduce :: Personaje -> Elemento -> Float 
danioQueProduce personaje elemento =salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje listaEnemigos = filter (puedeMatarlo personaje) listaEnemigos

puedeMatarlo :: Personaje -> Personaje -> Bool
puedeMatarlo personaje enemigo = any (elementoMortal personaje) (elementos enemigo)

elementoMortal :: Personaje -> Elemento -> Bool
elementoMortal personaje elemento = (==0) . salud . ataque elemento $ personaje


-- PUNTO 3

concentracion :: Int -> Elemento
concentracion nivelConcentracion = Elemento {tipo = "Magia", ataque = noHaceNada, defensa = aplicarMeditarNVeces nivelConcentracion }

noHaceNada :: Personaje -> Personaje
noHaceNada personaje = personaje

aplicarMeditarNVeces :: Int -> Personaje -> Personaje
aplicarMeditarNVeces valor = foldl1 (.) (replicate valor meditar)
 
esbirrosMalvados :: Int -> [Elemento] 
esbirrosMalvados cantidadEsbirros = replicate cantidadEsbirros esbirro

esbirro :: Elemento
esbirro = Elemento "Maldad" (causarDanio 1) noHaceNada

jack :: Personaje
jack = Personaje "jack" 300.0 [concentracion 3,katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = Elemento "Magia" (causarDanio 1000) noHaceNada

aku :: Int -> Float -> Personaje
aku anio cantidadSalud = Personaje "aku" cantidadSalud (concentracion 4 : (portalAlFuturo anio) : esbirrosMalvados (anio*100)) anio

portalAlFuturo :: Int -> Elemento
portalAlFuturo anio  = Elemento "Magia" (mandarAlAnio (anioFuturo anio)) (generarNuevoAku (anioFuturo anio))

anioFuturo :: Int -> Int
anioFuturo anio = anio + 2800

generarNuevoAku :: Int-> Personaje -> Personaje
generarNuevoAku anio personaje = mandarAlAnio anio personaje -- DUDA CON EL TEMA DE LA SALUD



-- PUNTO 4

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
    | estaMuerto atacante = (defensor,atacante)
    | otherwise           = luchar (aplicarElementos defensor (elementos atacante) ataque) (aplicarElementos atacante (elementos defensor) defensa)

 
estaMuerto :: Personaje -> Bool
estaMuerto personaje = (==0) . salud $ personaje

aplicarElementos personaje1 elementos funcion = foldl (aplicarUnElemento) personaje1 (map funcion elementos)

aplicarUnElemento :: Personaje -> (Personaje -> Personaje) -> Personaje
aplicarUnElemento personaje funcion = funcion personaje


