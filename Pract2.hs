--DEFINIR UNA FUNCION MAXIMO
maximo :: (Integer, Integer) -> Integer
maximo (x,y) = max x y

--FUNCION PAR
par :: Integer -> Bool
par x = (x `mod` 2)==0


--MAXIMO DE 3 NUMEROS
max3::(Integer, Integer, Integer)-> Integer
max3 (x, y, z)= max x (max y z)

--DEVUELVE SI EL NUMERO ES POSITIVO, CERO O NEGATIVO
sgn:: Int -> Int 
sgn x 
       | x >0 = 1
       | x == 0 = 0
       | x < 0  = -1


--VALOR ABSOLUTO
abso:: Int -> Int
abso x = abs x

--DETERMINA SI UN AÑO ES BISIESTO
bisiesto :: Int -> Bool
bisiesto n     | n `mod` 4 /= 0 = False
                | n `mod` 4 == 0 = n `mod` 100 /= 0 || n `mod` 400 == 0

--MEDIDAS DE LOS LADOS DE UN TRIANGULO
triangulo:: (Float, Float, Float) -> Bool
triangulo (a, b, c) = a <  b+c && b < a+c && c < a+b

--SI ES UN TRIANGULO RECTANGULO
es_rectangulo::(Integer, Integer, Integer) -> Bool
es_rectangulo (x, y, z) 
           |max3 (x, y, z) * max3 (x, y, z) == x*x + y*y + z*z - max3 (x, y, z) * max3 (x, y, z) = True
           | otherwise = False


dias=["lunes","martes","miercoles","jueves","viernes","sabado","domingo"]
--MUESTRA EL DIA SIGUIENTE
diasiguiente :: String -> String
diasiguiente a   | a == "lunes" = "martes"
                 | a == "martes" = "miercoles"
                 | a == "miercoles" = "jueves"
                 | a == "jueves" = "viernes"
                 | a == "viernes" = "sabado"
                 | a == "sabado" = "domingo"
                 | a == "domingo" = "lunes"
                 | otherwise = "No se encuentra el dia"

--DIAS QUE SE LABORA
labora :: String -> Bool
labora a  | a == "lunes" = True
          | a == "martes" = True
          | a == "miercoles" = True
          | a == "jueves" = True
          | a == "viernes" = True
          | a == "sabado" = False
          | a == "domingo" = False

--DIAS DE FIN DE SEMANA
finsemana :: String -> Bool
finsemana a  | a == "lunes" = False
             | a == "martes" = False
             | a == "miercoles" = False
             | a == "jueves" = False
             | a == "viernes" = False
             | a == "sabado" = True
             | a == "domingo" = True

--primer cambio 