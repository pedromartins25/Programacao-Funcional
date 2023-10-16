
module Ficha1 where
import Data.Char


-- EXERCICIO 3 (Continuação)
type Hora = (Int,Int)

minutosParaHora :: Int -> Hora
minutosParaHora m = divMod m 60

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras (h1,m1) (h2,m2) = abs(h1-h2) * 60 + abs(m1-m2)

adicionaMinutos :: Hora -> Int -> Hora
adicionaMinutos (h,m) min = (h + hr,mr)
    where (hr,mr) = minutosParaHora (m +min)


-- EXERCICIO 4
data Hora2 = H Int Int deriving (Show,Eq)

horaValida :: Hora2 -> Bool
horaValida (H h m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

horaDepoisDe :: Hora2 -> Hora2 -> Bool
horaDepoisDe (H h1 m1) (H h2 m2) = h1 > h2 || h1 == h2 && m1 > m2

horaParaMinutos :: Hora2 -> Int
horaParaMinutos (H h1 m1) = (h1 * 60) + m1

minutosHora :: Int -> Hora2
minutosHora m = H a b 
   where (a,b) = divMod m 60

diferencaHoras2 :: Hora2 -> Hora2 -> Int
diferencaHoras2 (H h1 m1) (H h2 m2) = (abs(h1-h2) * 60) + (abs(m2-m1))

adicionaMinutos2:: Hora2 -> Int -> Hora2
adicionaMinutos2 (H h m) min = H (h + hr) mr
   where (H hr mr) = minutosHora (m + min) 


-- EXERCICIO 5

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde 

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True
safe _ _ = False

-- EXERCICIO 6

data Ponto = Cartesiano Double Double
     | Polar Double Double
     deriving (Show,Eq)


posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = r * cos a

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = r * sin a

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x ^ 2 + y ^ 2)
raio (Polar r a) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) = atan (x/y)
angulo (Polar r a) = a

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((x' - x) ^2 + (y'-y)^2)
     where x = posx p1
           y = posy p1
           x' = posx p2
           y' = posy p2

-- EXERCICIO 7

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- EXERCICIO 8

isLower' :: Char -> Bool
isLower' c = ord c >= ord 'a' && ord c <= ord 'z'

isDigit' :: Char -> Bool
isDigit' c = ord c >= ord '1' && ord c <= ord '9'

isAlpha' :: Char -> Bool
isAlpha' c = isLower' c || ord c >= ord 'A' && ord c <= ord 'Z'

toUpper' :: Char -> Char
toUpper' c = if isLower' c then chr (ord c - 32) else c 

intToDigit' :: Int -> Char
intToDigit' n = chr ( n+48 )

digitToInt' :: Char -> Int
digitToInt' c = ord c - 48


