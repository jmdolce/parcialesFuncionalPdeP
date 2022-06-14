import Text.Show.Functions

-- PUNTO 1

data Turista = Turista{
    cansancio :: Int,
    stress    :: Int,
    viajaSolo :: Bool,
    idiomas   :: [Idiomas]
} deriving (Show)

type Idiomas = String

ana   = Turista 0 21 False ["español"]
beto  = Turista 15 15 True ["aleman"]
cathi = Turista 15 15 True ["aleman","catalan"]

type Excursion = Turista -> Turista

-- PUNTO 2

irALaPlaya :: Turista -> Turista
irALaPlaya turista 
    | viajaSolo turista = reducirCansancio 5 turista 
    | otherwise         = reducirStress 1 turista 

apreciarElementoPaisaje :: String -> Turista -> Turista
apreciarElementoPaisaje elemento turista = reducirStress (length elemento) turista 

hablarIdioma :: String -> Turista -> Turista
hablarIdioma idioma turista = cambiarAcompaniamiento . agregarIdioma idioma $ turista

caminar :: Int -> Turista -> Turista
caminar minutos turista = reducirStress (div minutos 4) . aumentarCansancio  (div minutos 4) $ turista

paseoEnBarco marea turista
    | marea == "fuerte"   = aumentarStress (6) . aumentarCansancio 10 $ turista
    | marea == "moderada" = turista
    | otherwise           = hablarIdioma ("aleman") . apreciarElementoPaisaje ("mar") . caminar 10 $ turista
    

agregarIdioma :: String -> Turista -> Turista
agregarIdioma idioma turista = turista {idiomas = idiomas turista ++ [idioma]}

cambiarAcompaniamiento :: Turista -> Turista
cambiarAcompaniamiento turista = turista {viajaSolo = False}

reducirCansancio :: Int -> Turista ->  Turista
reducirCansancio valor turista  = turista {cansancio = cansancio turista - valor}

reducirStress :: Int -> Turista -> Turista
reducirStress valor turista  = turista {stress = stress turista - valor}

aumentarCansancio :: Int -> Turista ->  Turista
aumentarCansancio valor turista  = turista {cansancio = cansancio turista + valor}

aumentarStress :: Int -> Turista -> Turista
aumentarStress valor turista  = turista {stress = stress turista + valor}


-- PUNTO 2.A


realizarExcursion :: Excursion -> Turista -> Turista
realizarExcursion excursion turista = reducirStress (div (stress turista) 10) .  excursion $ turista

-- PUNTO 2.B

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2


deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun indice turista excursion = deltaSegun indice (realizarExcursion excursion turista) turista


-- PUNTO 2.C

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = (>0) . deltaExcursionSegun  (length . idiomas)  turista $ excursion


excusionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excusionesDesestresantes turista listaDeExucusiones = filter (desestresa turista) listaDeExucusiones

desestresa :: Turista -> Excursion -> Bool
desestresa turista excursion = reduce3Unidades turista (deltaExcursionSegun stress turista  $ excursion) 

reduce3Unidades :: Turista -> Int -> Bool
reduce3Unidades turista valor = (stress turista - valor) >= 3



-- PUNTO 3

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElementoPaisaje "cascada", caminar 40, irALaPlaya, hablarIdioma "melmaquiano"]

ladoB :: Excursion -> Tour
ladoB  excursion  = [paseoEnBarco "tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina marea = [paseoEnBarco marea, excursionEnIsla marea, paseoEnBarco marea]


excursionEnIsla :: String -> Excursion
excursionEnIsla marea 
    | marea == "fuerte" = apreciarElementoPaisaje "lago"
    | otherwise        = irALaPlaya


-- PUNTO 3.A 

realizarTour :: Turista -> Tour -> Turista
realizarTour  turista tour = foldl (flip realizarExcursion) (aumentarStress (length tour) turista) tour

--- flipeo porque realizarExcursion recibe primero la excursion y despues al turista


-- PUNTO 3.B

esConvincente ::[Tour] -> Turista ->  Bool
esConvincente tours turista  = any (convenceTour turista) tours

convenceTour :: Tour -> Turista -> Bool
convenceTour tour turista = ((>0) . length . excusionesDesestresantes turista $ tour) && estaAcompaniado tour turista

estaAcompaniado :: Tour -> Turista -> Bool
estaAcompaniado tour turista = viajaSolo . realizarTour turista $ tour 


-- PUNTO 3.C

efectividad tour turistas = sum . map espiritualidad . filter (esConvincente tour) $ turistas

espiritualidad turista tour = calculoEspiritualidad . realizarTour turista $ tour

calculoEspiritualidad turista = stress turista + cansancio turista
