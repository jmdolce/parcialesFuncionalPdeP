import Text.Show.Functions

-- PUNTO 1

data Peleador = Peleador{
    puntosVida  :: Int,
    resistencia :: Int,
    ataques     :: [Ataque]
} deriving (Show)

type Ataque = Peleador -> Peleador

estaMuerto :: Peleador -> Bool
estaMuerto peleador = (<1) . puntosVida $ peleador 

esHabil :: Peleador -> Bool
esHabil peleador = (>10) . length . ataques $ peleador

golpe :: Int -> Ataque
golpe intensidad oponente = modificarVida (-) (div intensidad (resistencia oponente)) oponente

modificarVida :: (Int -> Int -> Int) -> Int -> Peleador -> Peleador
modificarVida funcion valor peleador = peleador {puntosVida = funcion (puntosVida peleador) valor}

toqueDeLaMuerte :: Ataque
toqueDeLaMuerte oponente = modificarVida (-) (puntosVida oponente) oponente

patada :: String -> Ataque
patada lugarCuerpo oponente 
    | lugarCuerpo == "pecho"  = determinarQueHace oponente
    | lugarCuerpo == "carita" = modificarVida (-) (div (puntosVida oponente) 2) oponente
    | lugarCuerpo == "nuca"   = olvidarAtaque oponente
    | otherwise               = oponente

determinarQueHace :: Peleador -> Peleador
determinarQueHace oponente 
    | estaMuerto oponente = modificarVida (+) 1 oponente
    | otherwise           = modificarVida (-) 10 oponente


olvidarAtaque :: Peleador -> Peleador
olvidarAtaque oponente = oponente {ataques = sacarPrimerAtaque . ataques $ oponente}

sacarPrimerAtaque :: [Ataque] -> [Ataque]
sacarPrimerAtaque ataques = drop 1 ataques

bruceLee :: Peleador
bruceLee = Peleador 200 25 [toqueDeLaMuerte,golpe 500, patada "carita" . patada "carita" . patada "carita"]


-- PUNTO 2

mejorAtaque :: Peleador -> Peleador -> Ataque
mejorAtaque peleador oponente = causaMasDanio oponente (ataques peleador)

causaMasDanio :: Peleador -> [Ataque] -> Ataque
causaMasDanio oponente [ataque] = ataque
causaMasDanio oponente (ataque1 : ataque2 : xs) 
    |  puntosVida (ataque1 oponente) > puntosVida (ataque2 oponente) = causaMasDanio oponente (ataque1:xs) 
    | otherwise                                                      = causaMasDanio oponente (ataque2:xs)


-- PUNTO 3

terrible :: Ataque -> [Peleador] -> Bool
terrible ataque listaEnemigos =(<(mitadListaEnemigos listaEnemigos)) . length . filter(not . estaMuerto) . map (realizarAtaque ataque) $ listaEnemigos

realizarAtaque :: Ataque -> Peleador -> Peleador
realizarAtaque ataque enemigo = ataque enemigo

mitadListaEnemigos :: [Peleador] -> Int
mitadListaEnemigos listaEnemigos = div (length listaEnemigos) 2

peligroso :: Peleador -> [Peleador] -> Bool
peligroso peleador listaEnemigos = all (flip terrible . filter (esHabil) $ listaEnemigos) (ataques peleador)