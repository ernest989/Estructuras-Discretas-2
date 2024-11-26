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
interpretacion (Atom x) l = valor x l
interpretacion (Neg f) l = not (interpretacion f l)
interpretacion (x :&: y) l = (interpretacion x l) && (interpretacion y l)
interpretacion (x :|: y) l = (interpretacion x l) || (interpretacion y l)
interpretacion (x :=>: y) l = not (interpretacion x l) || (interpretacion y l)
interpretacion (x :<=>: y) l = (interpretacion x l) == (interpretacion y l)

-- funcion auxiliar
-- valor busca el valor de verdad de una variable en la lista de pares ordenados
valor :: Var -> [(Var,Bool)] -> Bool
valor x [] = error "No todas las variables estan definidas"
valor x ((y, b):xs) = if x == y then b
    else valor x xs  
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones f = combinacionesAux (variables f)

--funcion auxiliar para tener acceso a la cabeza de a lista de vars 
combinacionesAux :: [Var] -> [[(Var, Bool)]]
combinacionesAux [] = [[]]
combinacionesAux (x:xs) = [(x,True) : c | c <- combinacionesAux xs ] ++ [(x, False) : c | c <- combinacionesAux xs]
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad f = [(x, interpretacion f x) | x <- combinaciones f]
-----------------------------------------------------



