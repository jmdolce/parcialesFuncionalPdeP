import Text.Show.Functions

-- PUNTO 1.A

data Postre = Postre{
    sabores     :: [Sabor],
    peso        :: Int,
    temperatura :: Int
} deriving (Show)

type Sabor = String

-- PUNTO 1.B

type Hechizos = [Hechizo]
type Hechizo = Postre -> Postre

torta :: Postre
torta = Postre ["choco"] 100 25

incendio :: Hechizo
incendio postre = modificarPeso (menosPorcentaje 5) . modificarTemperatura (+1) $ postre

modificarTemperatura :: (Int -> Int) -> Postre -> Postre
modificarTemperatura funcion postre = postre {temperatura = funcion (temperatura postre)}

modificarPeso :: (Int -> Int) -> Postre -> Postre
modificarPeso funcion postre = postre {peso = funcion (peso postre)}

menosPorcentaje :: Int -> Int -> Int
menosPorcentaje valor peso = (-) peso (div (valor * peso ) 100) 

immobulus :: Hechizo
immobulus postre = modificarTemperatura (const 0) postre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = modificarSabores ("concentrado" :) . modificarPeso (menosPorcentaje 10) $ postre

modificarSabores :: ([String] -> [String]) -> Postre -> Postre
modificarSabores funcion postre = postre {sabores = funcion (sabores postre)}

diffindo :: Int -> Hechizo
diffindo porcentaje postre = modificarPeso (menosPorcentaje porcentaje) postre

riddikulus :: Sabor -> Hechizo
riddikulus sabor postre = modificarSabores (sabor:) postre

avadaKedavra :: Hechizo
avadaKedavra postre = modificarSabores (const []) . immobulus $ postre


-- PUNTO 1.C

listos :: [Postre] -> Hechizo -> Bool
listos listaPostres hechizo = estanListos . map (realizarHechizo hechizo) $ listaPostres 

estanListos :: [Postre] -> Bool
estanListos listaPostres = all (estaListo) listaPostres

estaListo :: Postre -> Bool
estaListo postre = ((>0) . peso $ postre) && ((>0) . length . sabores $ postre) && ((>0) . temperatura $ postre)

realizarHechizo :: Hechizo -> Postre -> Postre
realizarHechizo hechizo postre = hechizo postre


-- PUNTO 1.D

pesoPromedio :: [Postre] -> Int
pesoPromedio listaPostres = div (sum . map (peso) $ listaPostres) (length listaPostres)


-- PUNTO 2.A

data Mago = Mago{
    hechizosAprendidos :: Hechizos,
    cantHorrorcruxes :: Int
} deriving (Show)

claseDefensa :: Mago -> Postre -> Hechizo -> Mago
claseDefensa mago postre hechizo = modificarHechizosAprendidos (hechizo:) . mismoEfectoavadaKedavra (mago) . realizarHechizo hechizo $ postre

mismoEfectoavadaKedavra :: Mago -> Postre -> Mago
mismoEfectoavadaKedavra mago postre 
    | ((==0) . length . sabores $ postre) && ((==0) . temperatura $ postre) = modificarHorrorcruxes (+1) mago
    | otherwise                                                             = mago

modificarHorrorcruxes :: (Int -> Int) -> Mago -> Mago
modificarHorrorcruxes funcion mago = mago {cantHorrorcruxes = funcion (cantHorrorcruxes mago)}

modificarHechizosAprendidos :: (Hechizos -> Hechizos) -> Mago -> Mago
modificarHechizosAprendidos funcion mago = mago {hechizosAprendidos = funcion (hechizosAprendidos mago)}


-- PUNTO 2.B

mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = mejor postre (hechizosAprendidos mago)

mejor :: Postre -> Hechizos -> Hechizo
mejor postre [hechizo] = hechizo
mejor postre (x:y:xs)
    | cantidadSabores x postre > cantidadSabores y postre = mejor postre (x:xs)
    | otherwise                                           = mejor postre (y:xs)


cantidadSabores :: Hechizo -> Postre -> Int
cantidadSabores hechizo postre = length . sabores . realizarHechizo hechizo $ postre


-- PUNTO 3

listaInfinitaPostres :: Postre -> [Postre]
listaInfinitaPostres postre = postre : (listaInfinitaPostres postre)

magoConInfinitosHechizos :: Hechizo -> Mago -> Mago
magoConInfinitosHechizos hechizo mago = magoConInfinitosHechizos (hechizo) . modificarHechizosAprendidos (hechizo:) $ mago








