--Alumno: Fernández Pizzella, Valentín

module Library where
import PdePreludat

--PUNTO 1: Postres
-- A) Modelar postres

data Postre = UnPostre {
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
} deriving (Show, Eq)

bizcochoBorracho = UnPostre ["Fruta","Crema"] 100 25 --ejemplo de postre

--B) Modelar los hechizos 
type Hechizo = Postre -> Postre

--funciones auxiliares
modificarPeso :: Number -> Postre -> Postre
modificarPeso porcentaje postre = postre {peso = peso postre * (100-porcentaje)/100 }

modificarTemperatura :: Number -> Postre -> Postre
modificarTemperatura grados postre = postre {temperatura = temperatura postre + grados}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre {sabores = sabor:sabores postre}

sacarSabores :: Postre -> Postre
sacarSabores postre = postre {sabores = []}

--hechizos
incendio :: Hechizo
incendio = modificarPeso 5 . modificarTemperatura 1

immobilus :: Hechizo
immobilus postre = postre {temperatura = 0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . modificarPeso 10

diffindo :: Number -> Hechizo
diffindo porcentaje postre = modificarPeso porcentaje postre

riddikulus :: String -> Hechizo
riddikulus ingrediente postre = agregarSabor (reverse ingrediente) postre

avadaKedavra :: Hechizo
avadaKedavra = immobilus.sacarSabores


--C) Están listos?  
estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo postres = all (estaListo.hechizo) postres

estaListo :: Postre -> Bool
estaListo postre = peso postre > 0 && sabores postre /= [] && temperatura postre > 0

--D) Peso promedio de los postres listos
pesoPromedioListos :: [Postre] -> Number
pesoPromedioListos = calcularPromedio . map peso . filter estaListo

calcularPromedio :: [Number] -> Number
calcularPromedio pesos = sum pesos / length pesos


--PUNTO 2: MAGOS
data Mago = UnMago {
    horrocruxes :: Number,
    hechizosAprendidos :: [Hechizo]
}

--A) Práctica de un hechizo
practica :: Postre -> Hechizo -> Mago -> Mago
practica postre hechizo = aprenderHechizo hechizo . sumarHorrocruxes postre hechizo

aprenderHechizo :: Hechizo -> Mago -> Mago
aprenderHechizo hechizo mago = mago {hechizosAprendidos=hechizo:hechizosAprendidos mago}

sumarHorrocruxes :: Postre -> Hechizo -> Mago -> Mago
sumarHorrocruxes postre hechizo mago | igualQueAvada postre hechizo = mago {horrocruxes = horrocruxes mago+1}
                                     | otherwise = mago {horrocruxes=horrocruxes mago}

igualQueAvada :: Postre -> Hechizo -> Bool
igualQueAvada postre hechizo = hechizo postre == avadaKedavra postre

--B) Mejor hechizo de un Mago
mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = compararSabores postre (hechizosAprendidos mago)

compararSabores :: Postre -> [Hechizo] -> Hechizo
compararSabores postre [x] = x
compararSabores postre (x:y:xs) |esMejor postre x y = compararSabores postre (x:xs)
                                | otherwise = compararSabores postre (y:xs)
                                
esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length.sabores.hechizo1) postre > (length.sabores.hechizo2) postre


--PUNTO 3: INFINITA MAGIA
--A) Lista infinita de postres
type Postres = [Postre]

postresInfinitos :: Postres -> Postres
postresInfinitos postres = cycle postres

--Mago con infinitos hechizos

magoInfinito :: Mago -> Mago
magoInfinito mago = mago {hechizosAprendidos = cycle (hechizosAprendidos mago)}

--B) Mesa con infinitos postres, algún hechizo los deja listos?
{- Es imposible determinar si hay algún hechizo que deje listos los infinitos postres ya que para
cumplir con la condición de que esten listos, debe evaluar todos los postres, lo cual es imposible
en una lista infinita.
-}

--C) Mago con infinitos hechizos, se puede encontrar el mejor hechizo?
{- Al igual que en la consigna anterior, no es posible ncontrar el mejor hechizo de un  mago con hechizos
infinitos, ya que la función mejorHechizo desarrollada en el punto 2.B requiere de evaluar por completo
la lista de hechizos y esto no es posible dado que la lista es infinita
-}