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
eliminarIndice xs indice = if (indice >= 0 && indice <= ((longitud xs)-1) ) then eliminarIndiceAux xs indice
	else error "Indice fuera del rango permitido"

--funcion auxiliar con índice válido
eliminarIndiceAux :: List a -> Int -> List a
eliminarIndiceAux (Void) _ = (Void)
eliminarIndiceAux (Node cabeza (cola)) 0 = (cola)
eliminarIndiceAux (Node cabeza (cola)) i = (Node cabeza (eliminarIndice cola (i-1)))

--funcion principal que revisa si el indice es correcto
insertarIndice :: List a -> Int -> a -> List a
insertarIndice xs indice e =  if (indice >= 0 && indice <= ((longitud xs)-1) ) then insertarIndiceAux xs indice e
	else error "Indice fuera del rango permitido"

--funcion auxiliar que inserta el elemento
insertarIndiceAux :: List a -> Int -> a -> List a
insertarIndiceAux Void _ nuevo = Node nuevo Void  
insertarIndiceAux (Node cabeza cola) 0 nuevo = Node nuevo (Node cabeza cola)  
insertarIndiceAux (Node cabeza cola) n nuevo = Node cabeza (insertarIndiceAux cola (n - 1) nuevo) 

-- Función para obtener el primer elemento de la lista
primerElemento :: List a -> a
primerElemento (Node x _) = x

-- fucion para eliminar el primer elemento de la lista
eliminarPrimero :: List a -> List a
eliminarPrimero (Node _ cola) = cola
eliminarPrimero Void = Void  

-- funcion para agregar un elemento al final de la lista
agregarFinal :: a -> List a -> List a
agregarFinal nuevo Void = Node nuevo Void
agregarFinal nuevo (Node cabeza cola) = Node cabeza (agregarFinal nuevo cola)

-- funcion principal que recorre la lista n veces hacia la derecha
recorrerLista :: List a -> Int -> List a
recorrerLista lista 0 = lista  
recorrerLista Void _ = Void    
recorrerLista lista n = recorrerLista (agregarFinal (primerElemento lista) (eliminarPrimero lista)) (n - 1)