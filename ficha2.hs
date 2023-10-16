--Ficha 2
import Data.Char

-- Exercicio 1

dobros :: [Float] -> [Float]
dobros [] = []
dobros (x:xs) = x * 2 : dobros xs

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (x:xs)
                  | c == x = 1 + numOcorre c xs
                  | otherwise = numOcorre c xs

positivos :: [Int] -> Bool
positivos [] = True
positivos (x:xs)
                | x < 0 = False
                | otherwise = positivos xs

soPos :: [Int] -> [Int]
soPos [] = []
soPos (x:xs)
            | x < 0 = soPos xs
            | otherwise = x : soPos xs

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (x:xs) | x < 0 = x + somaNeg xs
               | otherwise = somaNeg xs

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt l@(h:t) | length l <= 3 = l
                | otherwise = tresUlt t 

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,y): t) = y : segundos t 

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((x,_):t)
                       | n == x = True
                       | otherwise = nosPrimeiros n t

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) = (a + t1 , b + t2, c + t3) 
    where (t1,t2,t3) = sumTriplos t


-- EXERCICIO 3


soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h: soDigitos t
                | otherwise = soDigitos t

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

nums :: String -> [Int]
nums [] = []
nums (x:xs) | isDigit x = digitToInt x : nums xs
            | otherwise = nums xs

-- EXERCICIO 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((coeficiente, grau):t)
                              | n == grau = 1 + conta n t
                              | otherwise = conta n t

grau :: Polinomio -> Int
grau [] = 0
grau ((_,g):t)
                | g > grau t = g
                | otherwise = grau t

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((c,g):t) | x == g = (c,g) : selgrau x t
                    | otherwise = selgrau x t

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,g):t)
      | g == 0 = deriv t
      | otherwise = (c * fromIntegral g, g-1) : deriv t

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,g):t) = (c * x) ^ g + calcula x t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,g):t) | c == 0 = simp t
               | otherwise = (c,g) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (cm,gm) ((c,g):t) = (cm * c, gm + g) : mult (cm,gm) t

normaliza:: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,g):t) = normalizaAux (c,g) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (cm,gm) ((c,g):t)
              | gm == g = (cm + c,g) : t
              | otherwise = (c,g) : normalizaAux (cm,gm) t 

soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = somaAux (c,g) (soma t p)

somaAux :: Monomio -> Polinomio -> Polinomio
somaAux (cm,gm) ((c,g):t)
        | gm == g = (cm + c,g) : t
        | otherwise = (c,g) : somaAux (cm,gm) t

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = (mult m p) ++ produto t p

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = ordenaAux m (ordena t)

ordenaAux :: Monomio -> Polinomio -> Polinomio
ordenaAux m [] = [m]
ordenaAux (cm, gm) ((c,g):t)
          | g > gm = (cm,gm) : (c,g) : t
          | otherwise = (c,g) : ordenaAux (cm,gm) t

equiv:: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)

              


