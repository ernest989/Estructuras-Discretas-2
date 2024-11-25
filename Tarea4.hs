-- Tarea 4 cifrado de julio cesar 
import Data.Char (toUpper)
mayusculas = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z']
minusculas = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','ñ','o','p','q','r','s','t','u','v','w','x','y','z']

-- Nota --
-- algunas funciones no son robustas, sin embargo el programa esta diseñado para usar el main y no las funciones solitas

--Funciones para la codificación
-- funG es la biyeccion g 
funG :: Char -> [Char] -> Int
funG c (x:xs) = if c == x then 0
	else 1 + (funG c xs)

-- invG es g ^ -1 
invG :: Int -> Char
invG i
  | i >= 0 && i < length mayusculas = mayusculas !! i
  | otherwise = error "Índice fuera de rango"

-- la codificaion de p con la llave k
codLetra :: Char -> Int -> [Char ]-> Char
codLetra p k abc = invG (mod ((funG p abc) + k) 27) 

-- la decodificacion de c con la llave k
decLetra :: Char -> Int  -> [Char] -> Char
decLetra c k abc = invG (mod ((funG c abc) - k) 27) 

-- funcion para decodificar con la llave k
decodifica :: [Char] -> Int -> [Char] -> [Char]
decodifica [] _  _ = []
decodifica (x:xs) k abc = (decLetra x k abc) : (decodifica xs k abc) 

--funcion para codificar con la llave k
codifica :: [Char] -> Int -> [Char] -> [Char]
codifica [] _  _ = []
codifica (x:xs) k abc = (codLetra x k abc) : (codifica xs k abc)

-- Funciones para el manejo de cadenas

--limpiar solo toma va a tomar los elementos de minusculas y mayuscula
limpiar :: String -> [Char] -> [Char]
limpiar texto abc = [toUpper c | c <- texto, c `elem` abc]

main :: IO()
main = do 
	putStrLn "Elige una opción: "
	putStrLn "1. Codificar un mensaje "
	putStrLn "2. Decodifica un mensaje"
	putStrLn "0. Salir del programa"
	entrada <- getLine 
	let opcion = read entrada :: Int
	case opcion of 
		0 -> putStrLn "Saliendo del programa"
		1 -> do
			-- pide datos 
			putStrLn "ingresa un mensaje para codificar"
			mensaje <- getLine
			let cad = limpiar mensaje (mayusculas ++ minusculas)
			putStrLn "ingresa una llave del 1 al 27"
			llave <- getLine
			let k = read llave :: Int
			--codificacion
			putStrLn ("Tu mensaje codificado con la llave " ++ llave ++ " es:")
			let mensajeCod = codifica cad k mayusculas
			putStrLn mensajeCod
			-- decodificacion del mensaje
			putStrLn ("ingresa una llave del 1 al 27 para decodificar el mensaje anterior")
			llave <- getLine
			let k = read llave :: Int
			putStrLn ("tu mensaje decodificado con la llave " ++ llave ++ " es:")
			putStrLn (decodifica mensajeCod k mayusculas)
			-- regresa al programa
			main
		2 -> do
			-- pide datos
			putStrLn "ingresa un mensaje para decodificar"
			mensaje <- getLine
			let cad = limpiar mensaje (mayusculas ++ minusculas)
			putStrLn "Ingresa una llave para decodificar"
			llave <- getLine
			let k = read llave :: Int
			--decodificacion
			putStrLn ("tu mensaje decodificado con la llave " ++ llave ++ " es:")
			putStrLn (decodifica cad k mayusculas)
			-- regresa al programa
			main

		_ -> do
			putStrLn "Opcion inválida"
			-- regresa al programa
			main

-- el mensaje decodificado es LLEGAMOSAFINDECURSOGRACIAS y la llave es 17  se puede probar ingresando a la opcion 2 del main 