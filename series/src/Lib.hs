import Text.Show.Functions

data Serie = Serie {
    nombre        :: String,
    actores       :: [Actor],
    presupuesto   :: Int,
    cantidadTemp  :: Int,
    rating        :: Float,
    estaCancelada :: Bool
} deriving (Show)


data Actor = Actor{
    nombreActor      :: String,
    sueldoPretendido :: Int,
    restricciones    :: [Restriccion]
} deriving (Show)

type Restriccion = String

gotham = Serie "gotham" [bruce,gordon,lucius,barbara] 150 5 9.8 False
bruce = Actor "bruce" 50 ["a","b","c"]
gordon = Actor "gordon" 60 ["a","b"]
lucius = Actor "lucius" 8 ["a","b","c"]
barbara = Actor "barbara" 1 ["a","b"] 


-- PUNTO 1

estaEnRojo :: Serie -> Bool
estaEnRojo serie = (>=presupuesto serie) . sum . map sueldoPretendido $ actores serie

esProblematica :: Serie -> Bool
esProblematica serie = (>3) . length . masDeUnarestriccion . map restricciones $ actores serie

masDeUnarestriccion :: [[Restriccion]] -> [[Restriccion]]
masDeUnarestriccion listaRestricciones = filter ((>1) . length) listaRestricciones


-- PUNTO 2

type Productor = Serie -> Serie

conFavoritismo :: Actor -> Actor -> Productor
conFavoritismo actor1 actor2 serie = modificarActores (agregarActores actor1 actor2) . modificarActores (sacarActores 2) $  serie

modificarActores :: ([Actor] -> [Actor]) -> Serie -> Serie
modificarActores funcion serie = serie {actores = funcion (actores serie)}

sacarActores :: Int -> [Actor] -> [Actor]
sacarActores valor listaActores = drop valor listaActores

agregarActores :: Actor -> Actor -> [Actor] -> [Actor]
agregarActores actor1 actor2 listaActores =   actor1 : (actor2 : listaActores)


timBurton :: Productor
timBurton serie = conFavoritismo jhonnyDepp helenaBohnamCarter serie

jhonnyDepp = Actor "Jhonny Depp" 20000000 []
helenaBohnamCarter = Actor "Helena Bohnam Carter" 15000000 []

gatoPardeitor :: Productor
gatoPardeitor serie = serie

estireitor :: Productor 
estireitor serie = modificarTemporadas (*) 2 serie

modificarTemporadas :: (Int -> Int -> Int) -> Int -> Serie -> Serie
modificarTemporadas funcion valor serie = serie{cantidadTemp = funcion (cantidadTemp serie) valor}

desespereitor :: Int -> Productor -> Productor
desespereitor cantidadDeVeces productor serie = (foldl1 (.) (replicate cantidadDeVeces productor)) serie

canceleitor :: Float -> Productor
canceleitor cifra serie 
    | estaEnRojo serie || (rating serie) < cifra = modificarEstadoSerie serie
    | otherwise                                  = serie

modificarEstadoSerie :: Serie -> Serie
modificarEstadoSerie serie = serie{estaCancelada = True}


-- PUNTO 3

bienestar :: Serie -> Int
bienestar serie
    | estaCancelada serie       = 0 
    | cantidadTemp serie > 4    = 5 
    | cantidadTemp serie < 4    = calcularBienestar (cantidadTemp serie) *2
    | cantidadActores serie < 10 = 3
    | cantidadActores serie > 10 = min (calcularBienestar (cantidadActores serie)) 2 


cantidadActores :: Serie -> Int
cantidadActores serie = length . actores $ serie

calcularBienestar :: Int -> Int
calcularBienestar valor = (10 - valor) 


-- PUNTO 4

productorEfectivo :: [Serie] -> [Productor] -> [Serie]
productorEfectivo listaSeries listaProductores = map (masEfectivo listaProductores) listaSeries

masEfectivo :: [Productor] -> Serie -> Serie
masEfectivo (productor:[]) serie = productor serie
masEfectivo (productor1 : productor2 : xs) serie
    | bienestar (productor1 serie) > bienestar (productor2 serie) = masEfectivo (productor1 : xs) serie
    | otherwise                                                   = masEfectivo (productor2 : xs) serie



-- PUNTO 6

esControvertida :: Serie -> Bool
esControvertida serie = not . cobraMasQueElSig $ (actores serie)

cobraMasQueElSig :: [Actor] -> Bool
cobraMasQueElSig [actor] = True
cobraMasQueElSig (actor1 : actor2 : xs) 
    | sueldoPretendido actor1 > sueldoPretendido actor2 = cobraMasQueElSig (actor2 : xs)
    | otherwise                                         = False

