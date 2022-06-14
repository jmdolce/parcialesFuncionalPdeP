import Text.Show.Functions

-- PUNTO 1

data Animal = Animal {
    coefIntelectual :: Int,
    especie         :: String,
    habilidades     :: [String]
} deriving (Show)


animal1 = Animal 10 "reptil" ["r", "b", "c"]
animal2 = Animal 20 "ave" ["q","b"]
animal3 = Animal 23 "elefante" ["a"]

animales = [animal1,animal2,animal3]

-- PUNTO 2

inteligenciaSuperior :: Int -> Animal -> Animal
inteligenciaSuperior coeficiente animal = modificarIntelecto (+coeficiente) animal

modificarIntelecto :: (Int -> Int) -> Animal -> Animal
modificarIntelecto funcion animal = animal {coefIntelectual = funcion (coefIntelectual animal)}

pinkificar :: Animal -> Animal
pinkificar animal = modificarHabilidades (const []) animal

modificarHabilidades :: ([String] -> [String]) -> Animal -> Animal
modificarHabilidades funcion animal = animal {habilidades = funcion (habilidades animal)}

superpoderes animal
    | especie animal == "elefante"                              = modificarHabilidades ("no tenerle miedo a los ratones" :) animal
    | especie animal == "raton" && coefIntelectual animal > 100 = modificarHabilidades ("hablar" :) animal
    | otherwise                                                 = animal



-- PUNTO 3

type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico animal = (elem ("hablar") . habilidades $ animal) && ((>60) . coefIntelectual $ animal)

noTanCuerdo :: Criterio
noTanCuerdo animal = (>2) . length . filter (pinkiesco) . habilidades $ animal

pinkiesco :: String -> Bool
pinkiesco habilidad = (palabraPinkiesca (drop 7 habilidad)) && ((=="hablar") . (take 6) $ habilidad)

palabraPinkiesca :: String -> Bool
palabraPinkiesca habilidad = ((>=1) . length . filter (esVocal) $ habilidad) && ((<=4) . length $ habilidad)

esVocal :: Char -> Bool
esVocal letra = elem letra listaVocales

listaVocales :: [Char]
listaVocales = ['a','e','i','o','u']


-- PUNTO 4

data Experimento = Experimento {
    transformaciones :: [Transformacion],
    criterioDeExito  :: Criterio
} deriving (Show)

type Transformacion = Animal -> Animal


experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento animal = (criterioDeExito experimento) (aplicarTransformaciones animal experimento)

aplicarTransformaciones :: Animal -> Experimento -> Animal
aplicarTransformaciones animal experimento = (foldl (aplicarTransformacion) animal (transformaciones experimento))

aplicarTransformacion :: Animal -> Transformacion -> Animal
aplicarTransformacion animal transformacion = transformacion animal


-- PUNTO 5.1
 
reporte queMuestra cantHabilidades listaAnimales listaHabilidades experimento = map queMuestra . animalesConHabilidad (cantHabilidades) listaHabilidades . map (flip aplicarTransformaciones experimento) $ listaAnimales

listadoCoeficientes :: [Animal] -> [String] -> Experimento -> [Int]
listadoCoeficientes listaAnimales listaHabilidades experimento = reporte coefIntelectual tieneAlgunaHabilidad listaAnimales listaHabilidades experimento

animalesConHabilidad funcion listaHabilidades listaAnimales = filter (funcion listaHabilidades) listaAnimales

tieneAlgunaHabilidad :: [String] -> Animal -> Bool
tieneAlgunaHabilidad listaHabilidades animal = any (tieneHabilidad animal) listaHabilidades

tieneHabilidad :: Animal -> String -> Bool
tieneHabilidad animal habilidad = elem habilidad (habilidades animal)


-- PUNTO 5.2

listadoEspecies :: [Animal] -> [String] -> Experimento -> [String]
listadoEspecies listaAnimales listaHabilidades experimento = reporte especie tieneTodas listaAnimales listaHabilidades experimento

tieneTodas :: [String] -> Animal -> Bool
tieneTodas listaHabilidades animal = all (tieneHabilidad animal) listaHabilidades


-- PUNTO 5.3

listadoCapacidades :: [Animal] -> [String] -> Experimento -> [String]
listadoCapacidades listaAnimales listaHabilidades experimento  = concat (reporte habilidades noTieneNinguna listaAnimales listaHabilidades experimento)

noTieneNinguna :: [String] -> Animal -> Bool
noTieneNinguna listaHabilidades animal = not . any (tieneHabilidad animal) $ listaHabilidades
