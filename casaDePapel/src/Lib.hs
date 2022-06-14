import Text.Show.Functions

data Ladron = Ladron{
    nombre :: String,
    habilidades :: [Habilidad],
    armas       :: [Arma]
} deriving (Show)

type Habilidad = String
type Arma      = Rehen -> Rehen

data Rehen = Rehen{
    nombreRehen       :: String,
    nivelComplot :: Int,
    nivelMiedo   :: Int,
    plan         :: [Plan]
} deriving (Show)

type Plan = Ladron -> Ladron

pistola :: Int -> Arma
pistola calibre rehen = modificarMiedo (+) (3* (length . nombreRehen $ rehen)) . modificarComplot (-) (5*calibre) $ rehen

ametralladora :: Int -> Arma
ametralladora balas rehen = modificarMiedo (+) (balas) . modificarComplot (div) 2 $ rehen

modificarComplot :: (Int -> Int -> Int) -> Int -> Rehen -> Rehen
modificarComplot funcion valor rehen = rehen {nivelComplot = funcion (nivelComplot rehen) valor}

modificarMiedo :: (Int -> Int -> Int) -> Int -> Rehen -> Rehen
modificarMiedo funcion valor rehen = rehen {nivelMiedo = funcion (nivelMiedo rehen) valor}

disparos :: Ladron -> Rehen ->  Arma
disparos ladron rehen =  masMiedo rehen (armas ladron)

masMiedo :: Rehen -> [Arma] -> Arma
masMiedo rehen [arma] = arma
masMiedo rehen (arma1:arma2:xs) 
    | (nivelMiedo . arma1 $ rehen) > (nivelMiedo . arma2 $ rehen) = masMiedo rehen (arma1 : xs)
    | otherwise                                                   = masMiedo rehen (arma2 : xs)


hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo ladron rehen 
    | nombre ladron == "Berlin" = modificarMiedo (+) (cantidadLetrasHabilidades ladron) rehen
    | nombre ladron == "Rio"    = modificarComplot (+) 20 rehen
    | otherwise                 = modificarMiedo (+) 10 rehen
 

cantidadLetrasHabilidades :: Ladron -> Int
cantidadLetrasHabilidades ladron = sum . map length . habilidades $ ladron

atacarAlLadron :: Rehen -> Plan
atacarAlLadron rehen ladron = modificarArmas (div (length . nombreRehen $ rehen) 10) ladron

modificarArmas :: Int -> Ladron -> Ladron
modificarArmas valor ladron = ladron {armas = quitarArmas valor (armas ladron)}

quitarArmas :: Int -> [Arma] -> [Arma]
quitarArmas valor listaArmas = drop valor listaArmas

esconderse :: Plan
esconderse ladron = modificarArmas (div (length . habilidades $ ladron) 3) ladron


-- PUNTO 1

tokio :: Ladron
tokio = Ladron "Tokio" ["Trabajo psicologico","Entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["Disfrazarse de linyera","Disfrazarse de payaso","Estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 [esconderse]

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 [esconderse, atacarAlLadron pablo]


-- PUNTO 2

esInteligente :: Ladron -> Bool
esInteligente ladron = (>2) . length . habilidades $ ladron

-- PUNTO 3

consigaArma :: Ladron -> Arma -> Ladron
consigaArma ladron nuevaArma = ladron {armas = nuevaArma : armas ladron}


-- PUNTO 4

intimidar metodoIntimidacion ladron rehen = metodoIntimidacion rehen ladron


-- PUNTO 5

--calmarLasAguas ladron listaRehenes =  map (disparos ladron) listaRehenes


-- PUNTO 6

escaparse :: Ladron -> Bool
escaparse ladron = any (=="Disfrazarse de") . map (take (14)) . habilidades $ ladron


-- PUNTO 7

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal listaLadrones listaRehenes = promedioComplot listaRehenes > promedioMiedo listaRehenes * cantidadArmas listaLadrones

promedioComplot :: [Rehen] -> Int
promedioComplot listaRehenes = div (sum . map nivelComplot $ listaRehenes) (length listaRehenes)

promedioMiedo :: [Rehen] -> Int
promedioMiedo listaRehenes = div (sum . map nivelMiedo $ listaRehenes) (length listaRehenes)

cantidadArmas :: [Ladron] -> Int
cantidadArmas listaLadrones = length . concat . map armas $ listaLadrones


-- PUNTO 8
{-
--seRebelan :: [Rehen] -> Ladron -> Ladron
seRebelan listaRehenes ladron = ejecutarPlanes ladron . listaPlanes . map (modificarComplot (-) 10) $ listaRehenes

--listaPlanes :: [Rehen] -> [Plan]
listaPlanes listaRehenes = concat . map plan $ listaRehenes

--ejecutarPlanes :: Ladron -> [Plan] -> Ladron
ejecutarPlanes ladron listaPlanes = foldl (realizarPlan ladron) listaPlanes

--realizarPlan :: Ladron -> Plan -> Ladron
realizarPlan ladron plan = plan ladron
-}

-- PUNTO 9

ejecutarPlanValencia listaLadrones 