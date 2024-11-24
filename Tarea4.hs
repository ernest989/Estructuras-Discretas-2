-- Tarea 4
-- cifrado de julio cesar 
import Data.Char --libreria para eliminar los espacios
import System.IO --libreria para leer del main

abcdario = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z']
nums = [0..26]

-- funG es la biyeccion g 
funG :: Char -> [Char] -> Int
funG c (x:xs) = if c == x then 0
	else 1 + (funG c xs)

-- invG es g ^ -1 
invG :: Int -> [Char] -> Char
invG 0 (x:xs) = x
invG i (x:xs) = if i<0 then error "indice incorrecto"
	else invG (i-1) (x:xs)  

-- la codificaion de p con la llave k
codLetra :: Char -> Int -> Char
codLetra p k = invG (mod ((funG p nums) + k) 27) abcdario 

-- la decodificacion de c con la llave k
decLetra :: Char -> Int -> Char
decLetra c k = invG (mod ((funG c nums) - k) 27) abcdario

--funcion para eliminar espacios
elimSpace :: String -> String
elimSpace str = filter (not . isSpace) str

--funcion para pasar de String a [Char]
stringToLChar :: String -> [Char]
stringToLChar str = map toEnum str

--funcion para pasar de string a int
stringToInt :: String -> Int
stringToInt str = read str :: Int

-- funcion para decodificar con la llave k
decodifica :: [Char] -> Int -> [Char]
decodifica [] _ = []
decodifica (x:xs) k = (decLetra x k) : (decodifica xs k) 

--funcion para codificar con la llave k
codifica :: [Char] -> Int -> [Char]
codifica [] _ = []
codifica (x:xs) k = (codLetra x k) : (decodifica xs k)

