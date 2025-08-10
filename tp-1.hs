sucesor :: Int -> Int
sucesor n = n+1

sumar :: Int -> Int -> Int
sumar a b = a+b

divisionYResto :: Int -> Int -> (Int,Int)
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int,Int) -> Int
--Dado un par de números devuelve el mayor de estos.
maxDelPar (a , b) = if a > b
    then a
    else b

{-
2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
Ejemplo: maxDePar (divisionYResto (suma 5 5) (sucesor 0))
sucesor 9
sumar (maxDelPar (2,5)) (maxDelPar (2,5))
divisionYResto (sucesor 99) (sucesor 9)
maxDelPar ( (sumar 2 2), (sumar 5 5))
-}

--1. Denir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar
--las siguientes funciones:
data Dir = Norte | Sur | Este | Oeste deriving Show

opuesto :: Dir -> Dir
opuesto d = case d of 
    Norte -> Sur
    Este -> Oeste
    Oeste -> Este
    Sur -> Norte

--Dada una dirección devuelve su opuesta.
iguales :: Dir -> Dir -> Bool
--Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No hay para oeste"
--Dada una dirección devuelve su siguiente, en sentido horario, y suponiendo que no existe
--la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
--total o parcial? ¾Por qué
--al tener una precondicion significa que puede romper, en ese caso seria parcial

{-
2. Denir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones: -}
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

--primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana) 
primeroYUltimoDia = (Lunes, Domingo)

primerDia :: DiaDeSemana -> DiaDeSemana {-Subtarea para primeroYUltimoDia -}
primerDia _ = Lunes

ultimoDia :: DiaDeSemana -> DiaDeSemana {-Subtarea para primeroYUltimoDia -}
ultimoDia _ = Domingo

--Devuelve un par donde la primera componente es el primer día de la semana, y la
---segunda componente es el último día de la semana. Considerar denir subtareas útiles
--que puedan servir después.
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

valorDia :: DiaDeSemana -> Int
valorDia Lunes = 0
valorDia Martes = 1
valorDia Miercoles = 2
valorDia Jueves = 3
valorDia Viernes = 4
valorDia Sabado = 5
valorDia Domingo = 6
--Dado un día de la semana indica si comienza con la letra M.
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = valorDia d1 > valorDia d2
--Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
--la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
--analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Domingo = False
estaEnElMedio Lunes = False
estaEnElMedio _ = True
--Dado un día de la semana indica si no es ni el primer ni el ultimo dia.

negar :: Bool -> Bool
negar True = False
negar _ = True

{-
b) implica :: Bool -> Bool -> Bool
Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
devuelve True.
Esta función NO debe realizar doble pattern matching.
Nota: no viene implementada en Haskell.-}
implica :: Bool -> Bool -> Bool
implica False _ = True
implica _ d = d

{-
c) yTambien :: Bool -> Bool -> Bool
Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como \&\&.-}
yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien _ x = x

{-
d) oBien :: Bool -> Bool -> Bool
Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
Esta función NO debe realizar doble pattern matching.
En Haskell ya está definida como ||
-}

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ b = b

data Persona = P String Int deriving Show
martin = P "Martin" 21
franco = P "Franco" 31

nombre :: Persona -> String
--Devuelve el nombre de una persona
nombre (P n e) = n


edad :: Persona -> Int
--Devuelve la edad de una persona
edad (P n e) = e


crecer :: Persona -> Persona
--Aumenta en uno la edad de la persona.
crecer (P n e) = P n (e+1)


cambioDeNombre :: String -> Persona -> Persona
--Dados un nombre y una persona, devuelve una persona con la edad de la persona y el
--nuevo nombre.
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
--Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra per1 per2 = if edad per1 > edad per2
                            then True
                            else False

laQueEsMayor :: Persona -> Persona -> Persona
--Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor persona1 persona2 = if edad persona1 > edad persona2
                                  then persona1
                                  else persona2


{-Denir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego denir las
siguientes funciones:-}
data TipoDePokemon = Agua | Fuego | Planta deriving Show
 
data Pokemon = Poke TipoDePokemon Int deriving Show
 
data Entrenador = E String Pokemon Pokemon deriving Show

tipo :: Pokemon -> TipoDePokemon
tipo (Poke t e) = t

pok1 :: Entrenador -> Pokemon
pok1 (E n p1 p2) = p1

pok2 :: Entrenador -> Pokemon
pok2 (E n p1 p2) = p2

poke1 = Poke Planta 22
poke2 = Poke Fuego 21
poke3 = Poke Agua 1
poke4 = Poke Agua 11


yiy = E "Yiyo" poke1 poke2
tito= E "Tito" poke3 poke4

tipo_GanaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
tipo_GanaA_ Agua Fuego = True
tipo_GanaA_ Fuego Planta = True 
tipo_GanaA_ Planta Agua = True 
tipo_GanaA_ _ _ = False
--FUNC SUBTAREA PARA superaA, dados 2 tipos devuelve un bool si el primero vence al segundo

superaA :: Pokemon -> Pokemon -> Bool
--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA (Poke t e) (Poke t2 e2) = tipo_GanaA_ t t2



cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tipoBuscado (E n p p2) = sumarP tipoBuscado (tipo p) + sumarP tipoBuscado (tipo p2) 
--va a sumar +1 en cada uno de los pokemones si su tipo matchea con el "tipoBuscado"

--Dados dos tipos de pokemones delvolvera 1 si son igual 0 sino
sumarP :: TipoDePokemon -> TipoDePokemon -> Int
sumarP Planta Planta = 1
sumarP Fuego Fuego = 1
sumarP Agua Agua = 1
sumarP _ _ = 0

listaDeEntrenador :: Entrenador -> [Pokemon]
listaDeEntrenador (E n p p2) = [p,p2] 
--FUNC AUX para juntarPokemon, dado un entrenador devuelve sus pokemones en una lista

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
--Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (e1, e2) = listaDeEntrenador e1 ++ listaDeEntrenador e2

{-
5. Funciones polimórcas
1. Dena las siguientes funciones polimórcas:-}
loMismo :: a -> a
--Dado un elemento de algún tipo devuelve ese mismo elemento.
loMismo d = d

siempreSiete :: a -> Int
--Dado un elemento de algún tipo devuelve el número 7.
siempreSiete _ = 7

swap :: (a,b) -> (b, a)
--Dadas una tupla, invierte sus componentes.
swap (a,b) = (b,a)
{-¾Por qué existen dos variables de tipo diferentes?
2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimórcas?
   Porque funciona para cualquier tipo de dato que se le ingrese 
-}

{-6. Pattern matching sobre listas
1. Dena las siguientes funciones polimórcas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):-}

estaVacia :: [a] -> Bool
--Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
--Denida en Haskell como null.
estaVacia [] = True
estaVacia _  = False

elPrimero :: [a] -> a
--Dada una lista devuelve su primer elemento.
--Denida en Haskell como head.
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
--Dada una lista devuelve esa lista menos el primer elemento.
--Denida en Haskell como tail.
sinElPrimero (_:xs)= (xs)

splitHead :: [a] -> (a, [a])
--Dada una lista devuelve un par, donde la primera componente es el primer elemento de la
--lista, y la segunda componente es esa lista pero sin el primero.
splitHead (x) = (elPrimero x, sinElPrimero x)