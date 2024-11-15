-- @author Ernesto Ibrahim Medina De Luna 

data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud (Void) = 0
longitud (Node cabeza (cola)) = 1 + (longitud cola)

estaContenido :: Eq a => List a -> a -> Bool
estaContenido (Void) elem = False
estaContenido (Node cabeza (cola)) elem = if (cabeza==elem) then True
	else estaContenido (cola) elem  

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = (Void)
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista (Void) = []
convertirALista (Node cabeza (cola)) = (cabeza :(convertirALista cola)) 

--definicion de conjunto
conjunto :: Eq a => List a -> List a
conjunto (Void) = (Void) 
conjunto (Node cabeza (cola)) = if (estaContenido cola cabeza) then (conjunto cola)
	else (Node cabeza (conjunto cola))  

--funcion principal que revisa a condición
eliminarIndice :: List a -> Int -> List a
eliminarIndice (Void) _ = error "No hay elementos a eliminar"
eliminarIndice (Node cabeza (cola)) 0 = (cola)
eliminarIndice (Node cabeza (cola)) i = if 0<i && i > (longitud (Node cabeza (cola)) -1) 
	then error "Indice inválido"
	else (Node cabeza (eliminarIndice cola (i-1)))


-- inserta un elemento en el indice especificado de la lista
insertarIndice :: List a -> Int -> a -> List a
insertarIndice (Void) i elem = if i==0
	then (Node elem (Void)) 
	else error "Índice inváido"
insertarIndice (Node head tail) 0 elem = Node elem (Node head tail)
insertarIndice (Node head tail) i elem = if 0<i && i > (longitud (Node head (tail)) -1) 
	then error "Indice inválido"
	else Node head (insertarIndice tail (i-1) elem )

-- funcion principal que recorre la lista n veces hacia la derecha
recorrerLista :: List a -> Int -> List a
recorrerLista lista 0 = lista
recorrerLista Void _ = Void
recorrerLista (Node h Void) n = recorrerLista (Node h Void) (n - 1)
recorrerLista (Node h tail) n = if n<0 
	then error "parámetro inválido"
	else insertarIndice (recorrerLista tail (n - 1)) 0 h

