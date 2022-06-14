import Text.Show.Functions

data Persona = Persona {
    nombre   :: String,
    dinero   :: Float,
    suerte   :: Int,
    factores ::[(String,Int)]
 } deriving(Show)

nico = (Persona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)])
maiu = (Persona "Maiu" 100.0 42 [("inteligencia",55), ("paciencia",50)])


-- PUNTO 1

suerteTotal :: Persona -> Int
suerteTotal persona 
    | sirveAmuleto persona && sirveAmuleto persona = suerte persona * valorAmuleto persona
    | otherwise             = suerte persona


tieneAmuleto :: Persona -> Bool
tieneAmuleto persona = (>0) . length . filter ((=="amuleto") . fst) . factores $ persona

valorAmuleto :: Persona -> Int
valorAmuleto persona = snd . head . filter ((=="amuleto") . fst) . factores $ persona

sirveAmuleto :: Persona -> Bool
sirveAmuleto persona = (>0) . valorAmuleto $ persona


-- PUNTO 2

data Juego = Juego{
    nombreJuego  :: String,
    dineroGanado :: (Float -> Float),
    criterios    :: [(Persona -> Bool)]
}

ruleta apuesta     = Juego "ruleta" (apuesta (*37)) [((>80) . suerteTotal)]
maquinita apuesta  = Juego "maquinita" (apuesta (+jackpot)) [(((>95) . suerteTotal)),tenerPaciencia]

jackpot = 10 -- lo valido como 10 porque ni idea que es jackpot

tenerPaciencia :: Persona -> Bool
tenerPaciencia persona = elem ("paciencia") . map fst . factores $ persona


-- PUNTO 3

puedeGanar :: Persona -> Juego -> Bool
puedeGanar persona juego = all (cumpleCriterios persona) (criterios juego)

cumpleCriterios :: Persona -> (Persona -> Bool) -> Bool
cumpleCriterios persona criterio = criterio persona


-- PUNTO 4

-- PUNTO 5

noPuedenGanar :: [Persona] -> [Juego] -> [Persona]
noPuedenGanar listaPersonas listaJuegos = filter (noGanaNinguno listaJuegos) listaPersonas

noGanaNinguno :: [Juego] -> Persona -> Bool
noGanaNinguno listaJuegos jugador = all (puedeGanar jugador) listaJuegos

   
-- PUNTO 6

jugar :: Persona -> Float -> Juego -> Persona
jugar persona apuesta juego = ganaOPierde juego apuesta . restoInicial persona $ apuesta

restoInicial :: Persona -> Float -> Persona
restoInicial persona valor = persona {dinero = dinero persona - valor}

ganaOPierde :: Juego -> Float -> Persona -> Persona
ganaOPierde juego apuesta persona 
    | puedeGanar persona juego = persona{dinero = dinero persona + (dineroGanado juego apuesta)}
    | otherwise                = persona

