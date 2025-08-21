unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = unoSiCeroSino(e == x) + apariciones e xs

data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia

----------------------------------------------nroBolitas :: Color -> Celda -> Int--------------------------------------------------------------------
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas :: Color -> Celda -> Int
nroBolitas _ CeldaVacia = 0
nroBolitas color (Bolita c1 c) = unoSiCeroSino (esMismoColor color c1) + nroBolitas color c

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False

----------------------------------------------poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner :: Color -> Celda -> Celda
poner color cl = Bolita color cl


----------------------------------------------sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar _ CeldaVacia = CeldaVacia
sacar color (Bolita c1 c) = if esMismoColor color c1
                            then c else Bolita c1 (sacar color c) 

----------------------------------------------ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ celda = celda
ponerN n c celda = poner c (ponerN (n - 1) c celda)

----------------------------------------------1.2. Camino hacia el tesoro------------------------------------------------------------
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

----------------------------------------------hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino
hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Cofre objs cm) = hayTesoroEnLista objs || hayTesoro cm
hayTesoro (Nada cm) = hayTesoro cm

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

hayTesoroEnLista :: [Objeto] -> Bool
hayTesoroEnLista [] = False
hayTesoroEnLista (obj:objs) = esTesoro obj || hayTesoroEnLista objs

---------------------------------------------pasosHastaTesoro :: Camino -> Int
-- PRECOND: Tiene que haber al menos un Tesoro
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin = error "Debe haber por lo menos 1 tesoro en el camino"
pasosHastaTesoro (Cofre objs cm) = if hayTesoroEnLista objs then 0 else 1 + pasosHastaTesoro cm
pasosHastaTesoro (Nada cm) = 1 + pasosHastaTesoro cm

---------------------------------------------hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 c = esCofreConTesoro c
hayTesoroEn _ Fin = False
hayTesoroEn n (Cofre _ c) = hayTesoroEn (n - 1) c
----------------------------cuando llega a 0 verifica que haya tesoro
hayTesoroEn n (Nada c) = hayTesoroEn (n - 1) c

esCofreConTesoro :: Camino -> Bool
esCofreConTesoro (Cofre objs _) = hayTesoroEnLista objs
esCofreConTesoro _ = False

---------------------------------------------alMenosNTesoros
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _ = True
alMenosNTesoros _ Fin = False
alMenosNTesoros n (Nada cm) = alMenosNTesoros n cm
alMenosNTesoros n (Cofre objs cm) = alMenosNTesoros (n - cantTesorosEnLista objs) cm

cantTesorosEnLista :: [Objeto] -> Int
cantTesorosEnLista [] = 0
cantTesorosEnLista (obj:objs) = unoSiCeroSino(esTesoro obj) + cantTesorosEnLista objs

---------------------------------------------Desafio
cantTesorosEntre :: Int -> Int -> Camino -> Int
-- PRECOND: i<=j
cantTesorosEntre i j cm = cantTesorosDesde i (subCaminoHasta (j+1) cm)

cantTesorosEnCamino :: Camino -> Int
cantTesorosEnCamino Fin = 0
cantTesorosEnCamino (Cofre objs cm) = cantTesorosEnLista objs + cantTesorosEnCamino cm
cantTesorosEnCamino (Nada cm) = cantTesorosEnCamino cm

cantTesorosDesde :: Int -> Camino -> Int
-- PRECOND: i>=0
cantTesorosDesde 0 cm = cantTesorosEnCamino cm
cantTesorosDesde n Fin = 0
cantTesorosDesde n (Cofre objs cm) = cantTesorosDesde (n-1) cm
cantTesorosDesde n (Nada cm) = cantTesorosDesde (n-1) cm

subCaminoHasta :: Int -> Camino -> Camino
-- PRECOND: i>=0
subCaminoHasta 0 _ = Fin
subCaminoHasta _ Fin = Fin
subCaminoHasta n (Nada cm) = Nada (subCaminoHasta (n-1) cm)
subCaminoHasta n (Cofre objs cm) = Cofre objs (subCaminoHasta (n-1) cm)

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

treE = EmptyT

tree1 :: Tree Int
tree1 = NodeT 1 (NodeT 1 (NodeT 2 EmptyT EmptyT) EmptyT) EmptyT

tree2 :: Tree Int
tree2 = NodeT 1
        (NodeT 1 (NodeT 2 EmptyT EmptyT)
        (NodeT 2 EmptyT EmptyT)) EmptyT

tree3 :: Tree Int
tree3 = NodeT 10
        (NodeT 20 (NodeT 40 EmptyT EmptyT) (NodeT 50 EmptyT EmptyT))
        (NodeT 30 (NodeT 70 EmptyT EmptyT) (NodeT 70 EmptyT EmptyT))

tree4 :: Tree Char
tree4 = NodeT 'a'
          (NodeT 'b'
           (NodeT 'd' EmptyT EmptyT) EmptyT)
          (NodeT 'c'
           (NodeT 'e'
            EmptyT
            (NodeT 'g' EmptyT EmptyT))
           (NodeT 'f'
            (NodeT 'h' EmptyT EmptyT)
            (NodeT 'i' EmptyT EmptyT)))
-- Árboles binarios --
---------------------------------------------sumarT
sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT x lt rt) = x + sumarT lt + sumarT rt

---------------------------------------------sizeT
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x lt rt) = 1 + sizeT lt + sizeT rt

---------------------------------------------mapaDobleT
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x lt rt) = NodeT (2 * x) (mapDobleT lt) (mapDobleT rt)

---------------------------------------------perteneceT
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False
perteneceT x1 (NodeT x2 lt rt) = x1 == x2 || perteneceT x1 lt || perteneceT x1 rt

---------------------------------------------aparicionesT
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT x1 (NodeT x2 lt rt) = unoSiCeroSino(x1 == x2) + aparicionesT x1 lt + aparicionesT x1 rt

---------------------------------------------leaves
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x lt rt) = if esLeave lt rt
                         then [x] 
                         else leaves lt ++ leaves rt 

esLeave :: Tree a -> Tree a -> Bool
esLeave lt rt = esEmpty lt && esEmpty rt

esEmpty :: Tree a -> Bool
esEmpty EmptyT = True
esEmpty _ = False
---------------------------------------------heightT
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT _ lt rt) = 1 + max (heightT lt) (heightT rt)

---------------------------------------------mirrorT
mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x lt rt) = NodeT x (mirrorT rt) (mirrorT lt)

---------------------------------------------toList
toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT x lt rt) = toList lt ++ [x] ++ toList rt

---------------------------------------------levelN
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT x lt rt) = levelN (n-1) lt ++ levelN (n-1) rt

---------------------------------------------listPerLevel

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x lt rt) = [x] : concatenarHojasPorNivel (listPerLevel lt) (listPerLevel rt)

concatenarHojasPorNivel :: [[a]] -> [[a]] -> [[a]]
concatenarHojasPorNivel xss [] = xss
concatenarHojasPorNivel [] yss = yss
concatenarHojasPorNivel (xs:xss) (ys:yss) = (xs ++ ys) : concatenarHojasPorNivel xss yss

---------------------------------------------ramaMasLarga

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x lt rt) = x : listaMasLarga (ramaMasLarga lt) (ramaMasLarga rt)

listaMasLarga :: [a] -> [a] -> [a]
listaMasLarga xs ys = if length xs > length ys then xs else ys

---------------------------------------------todosLosCaminos
consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x lt rt) = [x] :
                                   consACada x (todosLosCaminos lt)
                                ++ consACada x (todosLosCaminos rt)

todosLosCaminosMaximales :: Tree a -> [[a]]
todosLosCaminosMaximales EmptyT = []
todosLosCaminosMaximales (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminosMaximales (NodeT x lt rt) = consACada x (todosLosCaminosMaximales lt ++ todosLosCaminosMaximales rt)

-- Expresiones Aritméticas --

data ExpA = Valor Int
        | Sum ExpA ExpA
        | Prod ExpA ExpA
        | Neg ExpA
        deriving Show

---------------------------------------------eval

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp) = (-1) * (eval exp)

---------------------------------------------simplificar
simplificar :: ExpA -> ExpA
simplificar (Sum exp1 exp2) = simplificarSuma (simplificar exp1) (simplificar exp2)
simplificar (Prod exp1 exp2) = simplificarProducto (simplificar exp1) (simplificar exp2)
simplificar (Neg exp) = simplificarNegativo(simplificar exp)
simplificar exp = exp

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) exp = exp
simplificarSuma exp (Valor 0) = exp
simplificarSuma exp1 exp2 = Sum exp1 exp2

simplificarProducto :: ExpA -> ExpA -> ExpA
simplificarProducto (Valor 0) exp = Valor 0
simplificarProducto (Valor 1) exp = exp
simplificarProducto exp (Valor 0) = Valor 0
simplificarProducto exp (Valor 1) = exp
simplificarProducto exp1 exp2 = Prod exp1 exp2

simplificarNegativo :: ExpA -> ExpA
simplificarNegativo (Neg exp) = exp
simplificarNegativo exp = Neg exp