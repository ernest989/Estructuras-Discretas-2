-- autor Ernesto Medina
-- Al compilar se mandan warnings pero son por mi editor de texto que añade "	" (tabulador) 

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = sqrt ( ((x2-x1)^2) + ((y2-y1)^2) )

hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt (b^2 + h^2)

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1,y1) (x2,y2) = if x2 == x1 then error "la pendiente no existe en ese punto"
	else (y2-y1)/(x2-x1)
 
raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = if a == 0 then error "a debe ser distinto a 0"
	else if (b^2)<(4*a*c) then error "b² debe ser mayor a 4ac" 
	else ( (-b + sqrt(b^2 - (4*a*c)))/ (2*a) , (-b - sqrt(b^2 - (4*a*c)))/ (2*a) )

areaTriangulo :: Float -> Float -> Float -> Float 
areaTriangulo a b c = sqrt (s * (s-a) * (s-b) * (s-c)) where
	s = auxSemiperimetro a b c 

-- el where es para que no se viera tan sucia la función jaja, soy recursador y vi haskell en modelado y progra,
-- voy viendo el último punto que no se pueden usar cosas extra, aún asi mañana (30 ago) te comento

-- Función auxiliar para calcular el semiperímetro
auxSemiperimetro :: Float -> Float -> Float -> Float
auxSemiperimetro a b c = (a+b+c)/2

comparador :: Int -> Int -> Int
comparador x y = if x==y then 0
	else if x < y then -1
		else 1

maximo :: Int -> Int -> Int -> Int
maximo x y z = if x>y && x>z then x
	else if y>x && y>z then y 
		else z

-- haciendo las pruebas vi que esDescendente 128 7 3 0 daba False y que esDescendente 0 3 7 128 daba True
-- así que vi que vi que hskell toma las entradas de derecha a izquierda (el orden original en la def de la funcion era a b c d)
esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente d c b a = if a<b && b<c && c<d then True 
	else False

