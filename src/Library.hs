module Library where
import PdePreludat

--Defino mis tipos
type Sabores = String
type Peso = Number
type GradoCentigrado = Number
type Hechizo = Postre -> Postre
type HechizoNumber = Postre -> Number -> Postre
type HechizoString = Postre-> String -> Postre
type Mesa = [Postre]

data Postre = UnPostre{
    sabores :: Sabores,
    peso :: Peso,
    temperatura :: GradoCentigrado
} deriving (Show, Eq, Ord)

data Mago = UnMago{
    hechizosAprendidos :: [Hechizo],
    cantidadHorrorcruxes :: Number
} deriving Show

--Defino mis postres
torta :: Postre
torta = UnPostre "Chocolate Fruta" 1 20

tarta :: Postre
tarta = UnPostre "Banana DulceDeLeche Crema" 2 20

helado :: Postre
helado = UnPostre "Frutilla Limon Avellana" 0.5 0

nadaCaliente :: Postre
nadaCaliente = UnPostre "" 1 100

mesa :: Mesa
mesa = [torta, tarta, helado]

mesaInfinita :: Mesa
mesaInfinita = repeat torta

--Defino mis hechizos
incendio :: Hechizo
incendio postre = postre {peso = peso postre * 0.95, temperatura = temperatura postre + 1}

immobulus :: Hechizo
immobulus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre {sabores = sabores postre++" Concentrado", peso = peso postre *0.9} 

diffindo :: HechizoNumber
diffindo postre porcentaje = postre {peso = peso postre * porcentaje}

riddikulus :: HechizoString
riddikulus postre sabor = postre {sabores = sabores postre++" "++reverse(sabor)}

avadaKedavra :: Hechizo
avadaKedavra postre = postre {sabores = "", temperatura = 0}

--Defino mis magos
harryPotter :: Mago
harryPotter = UnMago [incendio, immobulus] 1

tomRiddle :: Mago
tomRiddle = UnMago [incendio, avadaKedavra] 7

hermione :: Mago
hermione = UnMago [incendio, wingardiumLeviosa] 0

magoInfinito :: Mago
magoInfinito = UnMago (repeat incendio) 0

--Funciones para delegar
pesaAlgo :: Postre -> Bool
pesaAlgo postre = peso postre > 0

tieneSabores :: Postre -> Bool
tieneSabores postre = sabores postre /= ""

noEstaCongelado :: Postre -> Bool
noEstaCongelado postre = temperatura postre > 0

estaListo :: Postre -> Bool
estaListo postre = pesaAlgo postre && tieneSabores postre && noEstaCongelado postre

conocerListos :: Mesa -> [Bool] -> [Postre]
conocerListos [] _ = []
conocerListos (x:xs) (j:js)   
    |j = x:conocerListos xs js
    |otherwise = conocerListos xs js

obtenerPeso :: Postre -> Peso
obtenerPeso postre = peso postre

obtenerSabores :: Postre -> Sabores
obtenerSabores postre = sabores postre

calcularPromedio :: [Postre] -> Peso
calcularPromedio postres = sum(map obtenerPeso postres)/length postres

agregarHorrorcrux :: Hechizo -> Postre -> Number
agregarHorrorcrux hechizo postre
    |obtenerSabores(hechizo postre) == obtenerSabores(avadaKedavra postre) = 1
    |otherwise = 0

agregarHechizoYHorrorcrux :: Mago -> Hechizo -> Postre -> Mago
agregarHechizoYHorrorcrux mago hechizo postre = mago {hechizosAprendidos = hechizo:hechizosAprendidos mago, cantidadHorrorcruxes = cantidadHorrorcruxes mago + agregarHorrorcrux hechizo postre}

aplicarHechizos :: Postre -> [Hechizo] -> [Postre]
aplicarHechizos postre [] = []
aplicarHechizos postre (x:xs) = x postre:aplicarHechizos postre xs

separarSabores :: Sabores -> String
separarSabores "" = ""
separarSabores (x:xs) 
    |x == head(" ") = x:separarSabores xs
    |otherwise = separarSabores xs

mayorCantidadSabores :: [Postre] -> [Number]
mayorCantidadSabores postres = map (length.separarSabores.obtenerSabores) postres

elegirMejorHechizo :: [Hechizo] -> [Number] -> [Hechizo]
elegirMejorHechizo _ [] = []
elegirMejorHechizo (x:xs) (j:js)
    |all (<j) js = x:elegirMejorHechizo xs js
    |otherwise = elegirMejorHechizo xs js

--Funciones del programa
estanListos :: Mesa -> Hechizo -> [Bool]
estanListos mesa hechizo = map (estaListo.hechizo) mesa

pesoPromedioListos :: Mesa -> Hechizo -> Peso
pesoPromedioListos mesa hechizo = calcularPromedio (conocerListos mesa (estanListos mesa hechizo))

asistirAClase :: Mago -> Hechizo -> Postre -> Mago
asistirAClase mago hechizo postre = agregarHechizoYHorrorcrux mago hechizo postre

mejorHechizo :: Postre -> Mago -> [Hechizo]
mejorHechizo postre mago = elegirMejorHechizo (hechizosAprendidos mago) (mayorCantidadSabores (aplicarHechizos postre (hechizosAprendidos mago)))
