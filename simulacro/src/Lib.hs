import Text.Show.Functions
import Data.Char

-- PUNTO 1

data Barbaro = Barbaro{
    nombre      :: String,
    fuerza      :: Int,
    habilidades :: [String],
    objetos     :: [Objeto]
}  deriving (Show)

type Objeto = Barbaro -> Barbaro

dave = Barbaro "dave" 10 ["gola","chau"] [espada]

espada :: Objeto
espada barbaro = modificarFuerza (*2) barbaro

modificarFuerza :: (Int -> Int) -> Barbaro -> Barbaro
modificarFuerza funcion barbaro = barbaro {fuerza = funcion (fuerza barbaro)} 

amuletosMisticos :: String -> Objeto
amuletosMisticos habilidad barbaro = modificarHabilidades (habilidad :) barbaro

modificarHabilidades :: ([String] -> [String]) -> Objeto
modificarHabilidades funcion barbaro = barbaro {habilidades = funcion (habilidades barbaro)}

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro = modificarHabilidades ("Magia" :) . modificarObjetos (const []) $ barbaro

modificarObjetos :: ([Objeto] -> [Objeto]) -> Objeto
modificarObjetos funcion barbaro = barbaro {objetos = funcion (objetos barbaro)}

ardilla :: Objeto
ardilla barbaro = barbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 barbaro = objeto1 . objeto2 $ barbaro


-- PUNTO 2

megafono :: Objeto
megafono barbaro = modificarHabilidades (const [ (map (toUpper) . concat . habilidades $ barbaro) ] ) barbaro

megafonoBarbarico :: Objeto
megafonoBarbarico barbaro = cuerda ardilla megafono barbaro


-- PUNTO 3

type Aventura = [Evento]
type Evento = Barbaro -> Bool
type Prueba = Barbaro -> Bool

invasionDeSuciosduendes :: Evento
invasionDeSuciosduendes barbaro = elem ("Escribir Poesia Atroz") (habilidades barbaro)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = nombre barbaro == "Faffy" || nombre barbaro == "Astro"

ritualDefechorias :: [Prueba] -> Evento
ritualDefechorias pruebas barbaro = any (pasaPrueba barbaro) pruebas 

pasaPrueba :: Barbaro -> Prueba -> Bool
pasaPrueba barbaro prueba = prueba barbaro


-- PUNTO 4

eliminarRepetidos :: Eq a => [a] -> [a]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs)
    | elem x xs = eliminarRepetidos xs
    | otherwise = x : eliminarRepetidos xs


descendiente :: Barbaro -> Barbaro
descendiente barbaro = nombreConAsterisco . habilidadesSinRepetir . cargarObjetos . aplicarObjetos $ barbaro

aplicarObjetos :: Barbaro -> Barbaro
aplicarObjetos barbaro = foldl (aplicarObjeto) barbaro (objetos barbaro)

aplicarObjeto :: Barbaro -> Objeto -> Barbaro
aplicarObjeto barbaro objeto = objeto barbaro

cargarObjetos :: Barbaro -> Barbaro
cargarObjetos barbaro = modificarObjetos (objetos barbaro ++) barbaro

habilidadesSinRepetir :: Barbaro -> Barbaro
habilidadesSinRepetir barbaro = modificarHabilidades (eliminarRepetidos) barbaro

nombreConAsterisco :: Barbaro -> Barbaro
nombreConAsterisco barbaro = modificarNombre (++"*") barbaro

modificarNombre :: (String -> String) -> Barbaro -> Barbaro
modificarNombre funcion barbaro = barbaro { nombre = funcion (nombre barbaro)}

