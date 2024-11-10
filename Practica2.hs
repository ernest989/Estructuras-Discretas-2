--Ernesto Ibrahim Medina De Luna
--------------- Listas y recursión ---------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento l elem f = 
	if f == True then elem : l
	else l ++ [elem] 

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [] = error "La lista vacía no tiene máximo"
maximoLista [a] = a
maximoLista (x:xs) = max x (maximoLista xs)

indice :: [a] -> Int -> a
indice [] _ = error "La lista vacía no tiene elementos"
indice (x:xs) i =  
    if i==0 then x
    else if ( (0<=i) && (i< (longitud (x:xs))-1) ) then indice xs (i-1)
    else error "el indice es incorrecto"

--------------- Listas por comprehensión ---------------

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n] , (mod n x) == 0]

conjunto :: Eq a => [a] -> [a]
conjunto []= [] 
conjunto (x:xs) = x:[z | z <- (conjunto xs), z/=x]

numerosPares :: [Int] -> [Int]
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
