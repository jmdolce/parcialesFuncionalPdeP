import Text.Show.Functions

-- PUNTO 1

data Chofer = Chofer{
    nombre      :: String,
    kilometraje :: Int,
    viajes      :: [Viaje],
    condicion   :: Condicion
} deriving (Show)

type Condicion = Viaje -> Bool

data Viaje = Viaje{
    fecha          :: (Int,Int,Int),
    clienteQueTomo :: Cliente,
    costo          :: Int
} deriving (Show)

data Cliente = Cliente{
    nombreCliente :: String,
    dondeVive     :: String
} deriving (Show)


-- PUNTO 2

cualquierViaje :: Condicion
cualquierViaje _ = True

viajeMasDe200 :: Condicion
viajeMasDe200 viaje = (>200) . costo $ viaje

nombreConMasDeNLetras :: Int -> Condicion
nombreConMasDeNLetras valor viaje  = (> valor) . length . nombreCliente . clienteQueTomo $ viaje  

noVivaEnZonaDeterminada :: String -> Condicion
noVivaEnZonaDeterminada zona viaje = (=="zona") . dondeVive . clienteQueTomo $ viaje


-- PUNTO 3

lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

daniel :: Chofer
daniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) lucas 150] (noVivaEnZonaDeterminada "Olivos")

alejandra :: Chofer
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje


-- PUNTO 4

puedeTomarViaje ::Viaje -> Chofer ->  Bool
puedeTomarViaje viaje chofer = (condicion chofer) viaje


-- PUNTO 5

liquidacion :: Chofer -> Int
liquidacion chofer = sum . map costo . viajes $ chofer


-- PUNTO 6

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje listaChoferes = hacerViaje(viaje) . choferConMenosViajes . filter (puedeTomarViaje viaje) $ listaChoferes 

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes listaChoferes = foldl (menosViajes) (head listaChoferes) (tail listaChoferes)

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes chofer1 chofer2 
    | (length . viajes $ chofer1) > (length . viajes $ chofer2) = chofer1
    | otherwise                                                 = chofer2


hacerViaje :: Viaje -> Chofer -> Chofer
hacerViaje nuevoViaje chofer = chofer {viajes = nuevoViaje : viajes chofer}


-- PUNTO 7

nitoInfy :: Chofer
nitoInfy = Chofer "Nito Infy" 70000 (repetirViaje viajeRepetido) (nombreConMasDeNLetras 3)


repetirViaje viaje = viaje : repetirViaje viaje

viajeRepetido :: Viaje
viajeRepetido = Viaje (11,03,2017) lucas 50