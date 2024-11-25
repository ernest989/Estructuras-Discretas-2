-- libreria para usar sort
import Data.List

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom x) = [x]
variables (Neg f) = variables f
variables (x :&: y) = conjunto (variables x ++ variables y)
variables (x :|: y) = conjunto (variables x ++ variables y)
variables (x :=>: y) = conjunto (variables x ++ variables y)
variables (x :<=>: y) = conjunto (variables x ++ variables y)

-- funcion auxiliar conjunto
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = if x `elem` xs then conjunto xs
                  else x : conjunto xs
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom x) = Neg (Atom x)
negacion (Neg f) = f
negacion (x :|: y) = (negacion x) :&: (negacion y)
negacion (x :&: y) = (negacion x) :|: (negacion y)
negacion (x :=>: y) = x :&: (negacion y)
negacion (x :<=>: y) = (x :&: (negacion y)) :|: ((negacion x) :&: y)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom x) = (Atom x)
equivalencia (Neg f) = negacion (equivalencia f)
equivalencia (x :=>: y) = equivalencia ((Neg x) :|: y )
equivalencia (x :<=>: y) = equivalencia ( ((Neg x) :|: y) :&: (x :|: (Neg y)) )
equivalencia (x :&: y) = equivalencia x :&: equivalencia y
equivalencia (x :|: y) = equivalencia x :|: equivalencia y
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion f l = 
    if iguales (variables f) (variables2 l) then evaluar (equivalencia f) l
    else error "No todas las variables están definidas"

-- funciones auxiliares
-- variables2 obtiene las variables de una lista de pares 
variables2 :: [(Var,Bool)] -> [Var]
variables2 [] = []
variables2 ((x,_): xs) = conjunto ( x : variables2 xs)

-- iguales recibe 2 listas y las ordena con sort para ver si tienen los mismos elementos
-- ya que pueden venir en distinto orden los elementos de variables y de variables2
iguales :: Ord a => [a] -> [a] -> Bool
iguales xs ys = sort xs == sort ys

-- evalua va sustituir la fórmula por la lógica de haskell
evaluar :: Formula -> [(Var,Bool)] -> Bool
evaluar (Atom x) l = valor x l
evaluar (Neg f) l = not (evaluar f l)
evaluar (x :&: y) l = (evaluar x l) && (evaluar y l)
evaluar (x :|: y) l = (evaluar x l) || (evaluar y l)

-- valor busca el valor de verdad de una variable en la lista de pares ordenados
-- nota : no hay caso con [] porque tendremos garantizado por iguales que siempre va estar la variable en los pares
valor :: Var -> [(Var,Bool)] -> Bool
valor x ((y, b):xs) 
  | x == y = b 
  | otherwise = valor x xs 
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined
-----------------------------------------------------



