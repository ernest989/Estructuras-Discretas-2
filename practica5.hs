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

data List a = Void | Node a (List a) deriving Show

-------------------- EJERCICIO 1 --------------------
variables :: Formula -> [Var]
variables (Atom x) = [x]
variables (x :&: y) = conjunto (variables x ++ variables y)
variables (x :|: y) = conjunto (variables x ++ variables y)
variables (x :=>: y) = conjunto (variables x ++ variables y)
variables (x :<=>: y) = conjunto (variables x ++ variables y)

-- auxiliar funcion conjunto
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = if x `elem` xs then conjunto xs
                  else x : conjunto xs

-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom x) = Neg (Atom x)
negacion (x :|: y) = (negacion x) :&: (negacion y)
negacion (x :&: y) = (negacion x) :|: (negacion y)
negacion (x :=>: y) = x :&: (negacion y)
negacion (x :<=>: y) = (x :&: (negacion y)) :|: ((negacion x) :&: y)
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom x) = (Atom x)
equivalencia (Neg x) = equivalencia (negacion(x)) 
equivalencia (x :=>: y) = equivalencia ((Neg x) :|: y )
equivalencia (x :<=>: y) = equivalencia ( ((Neg x) :|: y) :&: (x :|: (Neg y)) )
 
-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones _ = undefined
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad _ = undefined
-----------------------------------------------------



