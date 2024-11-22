-- Ernesto Ibrahim Medina De Luna 423109476

data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud (ArbolVacio) = 0
longitud (Raiz _ ArbolVacio ArbolVacio) = 1
longitud (Raiz _ izq der) = 1 + longitud(izq) + longitud(der)

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad (ArbolVacio) = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho (ArbolVacio) = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho(izq) + ancho(der)

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido ArbolVacio _ = []
recorrido (Raiz r ArbolVacio ArbolVacio) _ = [r]
recorrido (Raiz r izq der) InOrder  = recorrido izq InOrder ++ [r] ++ recorrido der InOrder
recorrido (Raiz r izq der) PreOrder  = [r] ++ recorrido izq PreOrder ++ recorrido der PreOrder
recorrido (Raiz r izq der) PosOrder  = recorrido izq PosOrder ++ recorrido der PosOrder ++ [r]

-------------------- EJERCICIO 5 --------------------
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = [[]]
niveles (Raiz r ArbolVacio ArbolVacio) = [[r],[]]
niveles (Raiz r izq der) = nivelesAux (Raiz r izq der) []
 
-- función auxiliar para niveles
nivelesAux :: Arbol a -> [[a]] -> [[a]]
nivelesAux ArbolVacio niveles = niveles
nivelesAux (Raiz r izq der) niveles = [r] : nivelesAux izq (nivelesAux der niveles)
 
-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a 
minimo ArbolVacio = error "El arbol vacío no tiene elementos"
minimo (Raiz r ArbolVacio ArbolVacio) = r
minimo (Raiz r ArbolVacio der) = min r (minimo der)
minimo (Raiz r izq ArbolVacio) = min r (minimo izq)
minimo (Raiz r izq der) = min r (min (minimo izq) (minimo der))

maximo :: Ord a => Arbol a -> a 
maximo ArbolVacio = error "El arbol vacío no tiene elementos"
maximo (Raiz r ArbolVacio ArbolVacio) = r
maximo (Raiz r ArbolVacio der) = max r (maximo der)
maximo (Raiz r izq ArbolVacio) = max r (maximo izq)
maximo (Raiz r izq der) = max r (max (maximo izq) (maximo der))

-------------------- EJERCICIO 7 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz r ArbolVacio ArbolVacio) x = 
	if r == x then ArbolVacio 
	else (Raiz r ArbolVacio ArbolVacio)
eliminar (Raiz r izq der) x = creArbol (nuevaLista (Raiz r izq der) x)

--funcion nueva lista convierte el arbol a una lista y le elimina las ocurrencias de x
nuevaLista :: Arbol a -> a -> [a]
nuevaLista ArbolVacio x = []
nuevalista (Raiz r izq der) x = eliminaLista (recorrido (Raiz r izq der) InOrder) x 

--función que elimina las ocurrencias de un elemento en una lista
eliminaLista :: Eq a => [a] -> a -> [a]
eliminaLista [] _ = []
eliminaLista (x:xs) e = 
	if x == e then eliminaLista xs e 
	else x : eliminaLista xs e


-- función auxiliar que crea un arbol a partir de una lista 
creArbol :: Ord a => [a] -> Arbol a
creArbol [] = ArbolVacio
creArbol [x] = Raiz x ArbolVacio ArbolVacio
creArbol (x:xs) = creArbolAux (x:xs) ArbolVacio

--función con la recursión de cola para creArbol
creArbolAux :: Ord a => [a] -> Arbol a -> Arbol a
creArbolAux [] arbol = arbol
creArbolAux (x:xs) arbol = 
	creArbolAux xs (insertar x arbol)


-- función auxiliar que va insertando elementos
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x ArbolVacio = Raiz x ArbolVacio ArbolVacio
insertar x (Raiz y izq der)
    | x < y  = Raiz y (insertar x izq) der
    | x > y  = Raiz y izq (insertar x der)
    | otherwise = Raiz y izq der  


