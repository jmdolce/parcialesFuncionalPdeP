import Text.Show.Functions

data Producto = Producto{
    descripcion  :: String,
    peligrosidad :: Int,
    componentes  :: [Componente]
} deriving (Show)

type Componente = String


-- PRODUCTOS PUNTO 1

buenaQuimica :: Producto -> Producto -> Bool
buenaQuimica producto1 producto2 =tieneComponentes producto1 (componentes producto2)

tieneComponentes :: Producto -> [Componente] -> Bool
tieneComponentes producto listaComponentes = all (estaElComponente listaComponentes) (componentes producto)

estaElComponente :: [Componente] -> Componente -> Bool
estaElComponente listaComponentes componente = elem componente listaComponentes


-- CALIDAD PUNTO 1

type DispositivoDeControl = [Sensor]
type Sensor = Producto -> Bool

habilitaONo :: Producto -> DispositivoDeControl -> Bool
habilitaONo producto dispositivoControl = (cantSensoresQueHabilitan producto dispositivoControl) > (cantSensoresQueRechazan (cantSensoresQueHabilitan producto dispositivoControl) dispositivoControl)

cantSensoresQueHabilitan :: Producto -> DispositivoDeControl -> Int
cantSensoresQueHabilitan producto dispositivoControl = length . filter (aplicarControl producto) $ dispositivoControl

aplicarControl :: Producto -> Sensor -> Bool
aplicarControl producto sensor = sensor producto 

cantSensoresQueRechazan :: Int -> DispositivoDeControl -> Int
cantSensoresQueRechazan valor dispositivoControl = (length dispositivoControl) - valor


-- PUNTO 2

agregarSensor :: Sensor -> DispositivoDeControl -> DispositivoDeControl
agregarSensor sensor dispositivoControl = sensor : dispositivoControl

-- PUNTO 2.A

sensorHabilitarTodo :: Sensor
sensorHabilitarTodo producto = True

-- PUNTO 2.B

sensorComponentes :: Int -> Sensor
sensorComponentes cantidadComponentes producto = (==cantidadComponentes) . length . componentes $ producto

-- PUNTO 2.C

sensorMultiple :: [Sensor] -> Sensor
sensorMultiple sensores producto = (== (length sensores)) . cantSensoresQueHabilitan producto $ sensores

-- PUNTO 2.D

sensorPeligrosidad :: Int -> Sensor
sensorPeligrosidad peligrosidadPermitida producto = (<peligrosidadPermitida) . peligrosidad $ producto

-- PUNTO 3

rechazadosAHabilitados :: [Producto] -> DispositivoDeControl -> DispositivoDeControl -> [Producto]
rechazadosAHabilitados listaProductos dispositivoControl nuevoDispositivo = filter (flip habilitaONo nuevoDispositivo) . filter (noHabilita dispositivoControl) $ listaProductos

noHabilita ::DispositivoDeControl -> Producto ->  Bool
noHabilita dispositivoControl producto = not . habilitaONo producto $ dispositivoControl

invertir :: [Sensor] -> [Sensor]
invertir sensores = map (not .) sensores

-- TESTs

nafta :: Producto
nafta = Producto "nafta" 7 ["Petroleo","Etanol"]

bolitasNaftalina :: Producto
bolitasNaftalina = Producto "naftalina" 10 ["Etanol","Petroleo","sustancia blanca"]

disControl :: DispositivoDeControl
disControl = [sensorComponentes 2, sensorPeligrosidad 10]

-- habilitaONo nafta disControl -> True

-- buenaQuimica nafta bolitasNaftalina -> True

-- agregarSensor habilitarTodo disControl -> [habilitarTodo,sensorComponentes 2, sensorPeligrosidad 10]

escalonetina = Producto "Escalonetina" 99 ["Acido","Dimariol","Depaultinina","Liomessil"] 

-- sensorMultiple disControl escalonetina -> False, que es lo que debe retornar ya que el sensor de peligrosidad rechaza al producto


-- Si hay un producto con componenetes infinitas no va poder ser habilita ya que no va a poder terminar de evaluar en ciertos casos
-- si el sensor lo habilita o no, como es el caso del sensorComponentes que necesita la longitud de la lista componentes. 
-- El sensor que si podria habilitar es el sensorHabilitarTodo o el sensorPeligrosidad y el sensorMultiple va a depender de que 
-- sensores recibe.