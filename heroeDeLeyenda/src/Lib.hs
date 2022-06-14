import Text.Show.Functions

-- PUNTO 1

data Heroe = Heroe{
    epiteto        :: String,
    reconocimiento :: Int,
    artefactos     :: [Artefacto],
    tareas         :: [Tarea]
} deriving (Show)

type Tarea = Heroe -> Heroe

data Artefacto = Artefacto{
    nombre :: String,
    rareza :: Int
} deriving (Show)


-- PUNTO 2

paseALaHistoria :: Heroe -> Heroe
paseALaHistoria heroe 
    | reconocimiento heroe > 1000 = modificarEpiteto "El mitico" $ heroe 
    | reconocimiento heroe >= 500 = agregarArtefacto lanzaDelOlimpo . modificarEpiteto "El magnifico" $ heroe
    | reconocimiento heroe >100   = agregarArtefacto xiphos . modificarEpiteto "Hoplita" $ heroe
    | otherwise                   = heroe

modificarEpiteto :: String -> Heroe -> Heroe
modificarEpiteto nuevoEpiteto heroe = heroe {epiteto = nuevoEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto artefacto heroe = heroe {artefactos = artefacto : artefactos heroe}

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50


-- PUNTO 3

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto artefacto heroe = agregarArtefacto artefacto . modificarReconocimiento (+) (rareza artefacto) $ heroe

modificarReconocimiento :: (Int -> Int -> Int) -> Int -> Heroe -> Heroe
modificarReconocimiento funcion valor heroe = heroe {reconocimiento = funcion valor (reconocimiento heroe)}

escalarElOlimpo :: Tarea
escalarElOlimpo heroe = agregarArtefacto relampagoDeZeus . desecharArtefactos . triplicarRarezaArtefactos . modificarReconocimiento (+) 500 $ heroe

triplicarRarezaArtefactos :: Heroe -> Heroe
triplicarRarezaArtefactos heroe = modificarArtefactos (map (triplicarRareza)) heroe

triplicarRareza :: Artefacto -> Artefacto
triplicarRareza artefacto = artefacto {rareza = 3* rareza artefacto}

modificarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe  -> Heroe
modificarArtefactos funcion heroe  = heroe {artefactos = funcion (artefactos heroe)}

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos heroe = modificarArtefactos (filter (not . esComun)) heroe

esComun :: Artefacto -> Bool
esComun artefacto = rareza artefacto > 1000

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "Relampago de zeus" 500


ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadCuadras heroe  = modificarEpiteto ("Gros" ++ (replicate cantidadCuadras 'o')) heroe

data Bestia = Bestia{
    nombreBestia :: String,
    debilidad    :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

matarBestia :: Bestia -> Tarea
matarBestia bestia heroe 
    | (debilidad bestia) heroe = modificarEpiteto ("El asesino de la " ++ (nombreBestia bestia)) heroe
    | otherwise                     = modificarArtefactos (drop 1) . modificarEpiteto "El cobarde" $ heroe


-- PUNTO 4

heracles = Heroe "Guardian del olimpo" 700 [pistola,relampagoDeZeus] [matarAlLeonDeNemea]

pistola = Artefacto "Pistola" 1000


-- PUNTO 5

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea heroe = matarBestia leonDeNemea heroe

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon De Nemea" epitetoMayorA20

epitetoMayorA20 :: Debilidad
epitetoMayorA20 = (>20) . length . epiteto


-- PUNTO 6

realizarTarea :: Heroe -> Tarea -> Heroe
realizarTarea heroe tarea = agregarTarea tarea (tarea heroe)

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea tarea heroe = heroe {tareas = tarea : (tareas heroe)}


-- PUNTO 7

presumir :: Heroe -> Heroe -> (Heroe,Heroe)
presumir heroe1 heroe2 
    | reconocimiento heroe1 > reconocimiento heroe2     = (heroe1,heroe2)
    | reconocimiento heroe2 > reconocimiento heroe2     = (heroe2,heroe1)
    | sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2 = (heroe1,heroe2)
    | sumatoriaRarezas heroe2 > sumatoriaRarezas heroe1 = (heroe2,heroe1)
    | otherwise                                         = presumir (realizarTareasDelOtro heroe1 heroe2) (realizarTareasDelOtro heroe2 heroe1)

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas heroe =sum . map rareza $ (artefactos heroe)

realizarTareasDelOtro :: Heroe -> Heroe -> Heroe
realizarTareasDelOtro heroe otroHeroe = foldl realizarTarea heroe (tareas otroHeroe)

-- PUNTO 8

--al no diferenciarse en ningun aspecto van a entrar en el otherwise y realizar las tareas 
-- del otro pero al no tener tareas van a quedar ambos heroes igual repitiendo el ciclo indefinidamente.

-- PUNTO 9

hacerLabor :: [Tarea] -> Heroe -> Heroe
hacerLabor labor heroe = foldl realizarTarea heroe labor

