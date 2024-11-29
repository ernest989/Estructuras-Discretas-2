--Ernesto Ibrahim Medina De Luna
--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento [] a _ = [a]
agregaElemento xs a True = a : xs
agregaElemento xs a False = xs ++ [a]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista vacía no tiene máximo"
maximoLista [a] = a
maximoLista [a,b] = if a > b then a else b
maximoLista (x:y:xs) = if x > y then maximoLista (x:xs) else 
    maximoLista (y:xs)

indice :: [a] -> Int -> a
indice [] _ = error "La lista vacía no tiene elementos"
indice (x:xs) 0 = x 
indice (x:xs) i = if ( (0<=i) && (i< (longitud (x:xs))-1) ) then indice xs (i-1) 
    else 
        error "el indice es incorrecto"

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores 0 = error "el 0 tiene infinitos divisores"
divisores n = [x | x <- [1..n] , (mod n x) == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto xs = conjuntoAux xs []

--funcion auxiliar para la recursión de cola de conjunto, recibe 2 listas y entrega una sin repetidos
--la primera es la original y en la segunda lista se van añadiendo los elementos que no estan en la segunda
conjuntoAux :: Eq a => [a] -> [a] -> [a]
conjuntoAux [] xs = xs
conjuntoAux (x:xs) ys = if elemento ys x then conjuntoAux xs ys
    else conjuntoAux xs (ys ++ [x])

--funcion auxiliar elemento, revisa si un elemento esta en una lista
elemento :: Eq a => [a] -> a -> Bool
elemento [] _ = False
elemento [x] e = if (e == x) then True else False
elemento (x:xs) e = if e==x then True
    else elemento xs e

numerosPares :: [Int] -> [Int]
numerosPares [] = error "La lista vacía no tiene numeros"
numerosPares xs = [ x | x <- xs , mod x 2 == 0]

--main de prueba 
main :: IO()
main = do
  print (longitud [1,2,3,4,5]) --debe imprimir 5
  print (sumaLista [4,5,5,6]) --debe imprimir 20
  print (agregaElemento ['a','b','c','d'] 'z' True) --debe agregar z al principio
  print (agregaElemento ['a','b','c','d'] 'w' False) --debe agregar w al final
  print (maximoLista [1,2,3,5,901,120,-1,900]) --debe dar 901
  print (indice [1,1,1,1,1,-2,1,2] 6) -- debe dar 1
  print (divisores 10) --debe dar 10,5,2,1
  print (conjunto ["bebe","carro", "carro", "Jimena", "ella", "ella", "ella"]) -- debe dar bebe,carro,mañana,ella
  print (numerosPares [3,7,20,-4,8,120,1]) --debe dar 20,-4,8,120