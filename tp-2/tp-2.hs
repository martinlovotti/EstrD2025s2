sumatoria :: [Int] -> Int
--Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria (xs)

longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
--de elementos que posee.
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

sucesor :: Int -> Int
sucesor n = n+1

sucesores :: [Int] -> [Int]
--Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores [] = []
sucesores (n:ns) = sucesor n : sucesores ns

conjuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion [] = True
conjuncion (b:bs) = b && conjuncion bs

disyuncion :: [Bool] -> Bool
--Dada una lista de booleanos devuelve True si alguno de sus elementos es True
disyuncion [] = False
disyuncion (b:bs) = b || disyuncion bs

aplanar :: [[a]] -> [a]
--Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (l:ls) = l ++ aplanar ls

pertenece :: Eq a => a -> [a] -> Bool
--Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
pertenece x [] = False
pertenece x (y:ys) = if x == y 
                        then True
                        else pertenece x ys

apariciones :: Eq a => a -> [a] -> Int
--Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones x [] = 0
apariciones x (y:ys) = if x==y 
                        then 1 + apariciones x ys
                        else 0 + apariciones x ys

losMenoresA :: Int -> [Int] -> [Int]
--Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n
losMenoresA _ [] = []
losMenoresA n (x:xs) = if n > x 
                        then x : losMenoresA n xs
                        else losMenoresA n xs

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más de n elementos.
lasDeLongitudMayorA _ [] = [[]]   
lasDeLongitudMayorA n (x:xs) = if (longitud x) > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x = [x]
agregarAlFinal (n:ns) x = n : agregarAlFinal ns x 

--12. agregar :: [a] -> [a] -> [a]
--Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
--elementos de la segunda a continuación. Denida en Haskell como (++)

agregar :: [a] -> [a] -> [a]
agregar [] ys = ys
agregar (x:xs) ys = x : agregar xs ys

--13 reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Denida
--en Haskell como reverse.

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x --Arreglado en clase



{-14. zipMaximos :: [Int] -> [Int] -> [Int]
Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
las listas no necesariamente tienen la misma longitud.
-}
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x:xs) (y:ys) = max x y : zipMaximos xs ys

{-15. elMinimo :: Ord a => [a] -> a
Dada una lista devuelve el mínimo
-}
elMinimo :: Ord a => [a] -> a
elMinimo [] = error "lista vacia"
elMinimo [x] = x
elMinimo (x:xs) = if x < elMinimo xs
                  then x
                  else elMinimo xs --ARREGLADO

{-
2. Recursión sobre números
Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
lo contrario:-}


--1. factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--2. cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n < 1
                    then []
                    else n : cuentaRegresiva (n-1)

--3. repetir :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n x = x : repetir (n - 1) x

--4. losPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--Si la lista es vacía, devuelve una lista vacía.
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n - 1) xs

--5. sinLosPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 

{-3. Registros
1. Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}

data Persona = P String Int 
 deriving Show

martin = P "Martin" 20
franco = P "Franco" 21
marcos = P "marcos" 22
pepe = P "Pepe" 23
manuel = P "manuel" 20
pes = [martin,marcos,manuel,pepe,franco]

edad :: Persona -> Int
--Devuelve la edad de una persona
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA _ [] = []
mayoresA n (p:ps) = if (edad p > n) 
                    then p : mayoresA n ps
                    else mayoresA n ps  

promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad [] = 0
promedioEdad ps = div (sumarEdades ps) (longitud ps) 

sumarEdades :: [Persona] -> Int
sumarEdades [] = 0
sumarEdades (x:xs) = edad x + sumarEdades xs

elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la lista al menos posee una persona.
elMasViejo [] = error "lista vacia"
elMasViejo [p] = p
elMasViejo (p:ps) = laQueEsMayor p (elMasViejo ps)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                        then p1
                        else p2                   
                   
{-
2. Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:-}

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

---------------------------------------------------EJEMPLOS-----------------------------------------------------------------
poke1 = ConsPokemon Planta 22
poke2 = ConsPokemon Fuego 21
poke3 = ConsPokemon Agua 1
poke4 = ConsPokemon Agua 11

pks = [poke1,poke2,poke3]

tito= ConsEntrenador "Tito" pks

--------------------------------------------------------OBSERVADORAS-----------------------------------------------------------------------------
pokemonesDelEntrenador :: Entrenador -> [Pokemon]
pokemonesDelEntrenador (ConsEntrenador n pks) = pks 

tipoDe :: Pokemon -> TipoDePokemon
tipoDe (ConsPokemon t e) = t
-------------------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------2.1---------------------------------------------------------------------------
cantPokemon :: Entrenador -> Int
--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (ConsEntrenador n pks) = longitud pks

------------------------------------------------------------2.2---------------------------------------------------------------------------
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe tipo (ConsEntrenador _ pokes) = sumarPokemonDe tipo pokes      --ARREGLADO

sumarPokemonDe :: TipoDePokemon -> [Pokemon] -> Int
sumarPokemonDe _ [] = 0
sumarPokemonDe tipoBuscado (pk:pks) = sumarP tipoBuscado (tipoDe pk) + sumarPokemonDe tipoBuscado pks 

--Dados dos tipos de pokemones delvolvera 1 si son igual 0 sino
sumarP :: TipoDePokemon -> TipoDePokemon -> Int
sumarP Planta Planta = 1
sumarP Fuego Fuego = 1
sumarP Agua Agua = 1
sumarP _ _ = 0


------------------------------------------------------------2.3---------------------------------------------------------------------------
cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ tipo (ConsEntrenador n pks) (ConsEntrenador n2 pks2) = 
                                     longitud (losQueGanan (losDeTipo tipo pks) pks2 )  

losDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
losDeTipo _ [] = []
losDeTipo tipo (x:xs) = if esTipo_ tipo (tipoDe x)
                        then x : losDeTipo tipo xs
                        else losDeTipo tipo xs 

losQueGanan :: [Pokemon] -> [Pokemon] -> [Pokemon]
losQueGanan [] _ = []
losQueGanan (x:xs) pks2 = if leGanaATodosDe x pks2 
                          then x : losQueGanan xs pks2
                          else losQueGanan xs pks2

leGanaATodosDe :: Pokemon -> [Pokemon] -> Bool
leGanaATodosDe _ [] = True
leGanaATodosDe x (p:ps) = superaA x p && leGanaATodosDe x ps  

tipo_GanaA_ :: TipoDePokemon -> TipoDePokemon -> Bool
tipo_GanaA_ Agua Fuego = True
tipo_GanaA_ Fuego Planta = True 
tipo_GanaA_ Planta Agua = True 
tipo_GanaA_ _ _ = False
--FUNC SUBTAREA PARA superaA, dados 2 tipos devuelve un bool si el primero vence al segundo

superaA :: Pokemon -> Pokemon -> Bool
--Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
--supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA (ConsPokemon t e) (ConsPokemon t2 e2) = tipo_GanaA_ t t2

------------------------------------------------------------2.4---------------------------------------------------------------------------
esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador n pks) = hay1DeCadaTipoEn pks --ARREGLADO

hay1DeCadaTipoEn :: [Pokemon] -> Bool
hay1DeCadaTipoEn pks = hayUn_ Fuego pks && hayUn_ Agua pks && hayUn_ Planta pks 

hayUn_ :: TipoDePokemon -> [Pokemon] -> Bool
--Evalua si existe algun pokemon con el tipo dado 
hayUn_ t [] = False
hayUn_ t (x:xs) = esTipo_ t (tipoDe x) || hayUn_ t xs

esTipo_ :: TipoDePokemon -> TipoDePokemon -> Bool
esTipo_ Agua Agua = True
esTipo_ Fuego Fuego = True
esTipo_ Planta Planta = True
esTipo_ _ _ = False

{-3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La denición es la siguiente:-}
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto nombre) = nombre

sonIguales :: Proyecto -> Proyecto -> Bool
sonIguales p1 p2 = if nombreProyecto p1 == nombreProyecto p2
                   then True
                   else False

quitarDuplicados :: [Proyecto] -> [Proyecto]
quitarDuplicados [] = []
quitarDuplicados (x:xs) = x : quitarDuplicados (quitar x xs)

quitar :: Proyecto -> [Proyecto] -> [Proyecto]
quitar _ [] = []
quitar x (y:ys) = if sonIguales x y
                  then quitar x ys
                  else y : quitar x ys

proyectos :: Empresa -> [Proyecto]
--Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos (ConsEmpresa r) = quitarDuplicados (proyectosEnTrabajo r)

proyectosEnTrabajo :: [Rol] -> [Proyecto]
--Devuelve un array con todos los proyectos de los que trabaja la empresa
proyectosEnTrabajo [] = []
proyectosEnTrabajo (x:xs) = proyectoDe x : proyectosEnTrabajo xs

proyectoDe :: Rol -> Proyecto --OBSERVADORA
proyectoDe (Developer _ proyecto) = proyecto
proyectoDe (Management _ proyecto) = proyecto

proA = ConsProyecto "Proyecto A"
proB = ConsProyecto "Proyecto B"
proC = ConsProyecto "Proyecto C"
proD = ConsProyecto "Proyecto D"
rol1 = Developer Senior proB
rol2 = Developer Senior proB
rol3 = Developer Senior proB

roles = [rol1 , rol2 , rol3]
proyectosE = [proB]
empresa = ConsEmpresa roles

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer s p) = p
proyectoDeRol (Management s p) = p

seniority :: Rol -> Seniority 
seniority (Developer s p) = s
seniority (Management s p) = s

rolDe :: Empresa -> [Rol] --OBSERVADORA
rolDe (ConsEmpresa p) = p


losDevSenior :: Empresa -> [Proyecto] -> Int
--Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen además a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa roles) ps = seniorsDe_ConProyecto_ roles ps

esDevSenior :: Rol -> Bool
esDevSenior rol = esDev rol && esSenior (seniority rol)

esDev :: Rol -> Bool
esDev (Developer _ _) = True
esDev _ = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _ = False

seniorsDe_ConProyecto_ :: [Rol] -> [Proyecto] -> Int
seniorsDe_ConProyecto_ [] _ = 0
seniorsDe_ConProyecto_ (r:rs) proyect = if (esDevSenior r && tiene_ElProyecto_ r proyect)
                                        then 1 + seniorsDe_ConProyecto_ rs proyect
                                        else seniorsDe_ConProyecto_ rs proyect

tiene_ElProyecto_ :: Rol -> [Proyecto] -> Bool
                                            -- disyuncionDeProyectos-- asi pertenece al menos a un proyecto de los dados por parametro  
tiene_ElProyecto_ rol todosLosProyectos = disyuncionDeProyectos (proyectoDe rol) todosLosProyectos
                                

disyuncionDeProyectos :: Proyecto -> [Proyecto] -> Bool 
disyuncionDeProyectos _ [] = False
disyuncionDeProyectos pro (x:xs) = es_DelProyecto_  pro (nombreProyecto x) || disyuncionDeProyectos pro xs


es_DelProyecto_ :: Proyecto -> String -> Bool
es_DelProyecto_ pro1 pro2 = compareStrings (nombreProyecto pro1) pro2

compareStrings :: String -> String -> Bool
compareStrings s1 s2 = s1 == s2


cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn todosProyectos (ConsEmpresa roles) = proyectos_ConRol_ todosProyectos roles

proyectos_ConRol_ :: [Proyecto] -> [Rol] -> Int
proyectos_ConRol_ _ [] = 0
proyectos_ConRol_ losProyectos (x:xs) = if disyuncionDeProyectos (proyectoDe x) losProyectos --Si matchea el rol con algun proyecto da 1
                                        then 1 + proyectos_ConRol_ losProyectos xs
                                        else 0 + proyectos_ConRol_ losProyectos xs


agregarProyectoAListaDePares :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoAListaDePares p [] = [(p, 1)]
agregarProyectoAListaDePares p ((pj,n) : pjns) = if es_DelProyecto_ p (nombreProyecto pj) 
                                                        then (pj, n + 1) : pjns
                                                        else (pj, n) : agregarProyectoAListaDePares p pjns

cantEmpleadosAsignadosPorProyecto :: [Rol] -> [(Proyecto, Int)]
cantEmpleadosAsignadosPorProyecto [] = []
cantEmpleadosAsignadosPorProyecto (r:rls) = agregarProyectoAListaDePares (proyectoDeRol r) (cantEmpleadosAsignadosPorProyecto rls)

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = cantEmpleadosAsignadosPorProyecto (rolDe e)

