--1

enumFromTo':: Int -> Int -> [Int]
enumFromTo' x y 
              | x >= y = [x]
              | otherwise = x : enumFromTo' (x+1) y

--2

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x c y
                | x+c >=y = [x+c-1]
                | otherwise = x: enumFromThenTo'(x+c-1) c y

--3
plusPlus :: [a] -> [a] -> [a]
plusPlus [] _ = []
plusPlus _ [] = []
plusPlus (h:t) (h1:t1) = h : h1 : plusPlus t t1

--4

pos':: [a] -> Int -> a
pos' (h:t) 0 = h
pos' (h:t) x = pos' t (x-1)

--5

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]

--6

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 l = []
take' x (h:t) = h : take' (x-1) t

--7

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' x (h:t) = drop' (x-1) t

--8

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (h1:t1) (h2:t2) = (h1,h2) : zip' t1 t2

--9
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

--10
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' x (h:t) = h : x : intersperse' x t

--11
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:takeWhile (==h) t) : group' (dropWhile (==h) t)

--12

concat':: [[a]] -> [a]
concat' [] = []
concat' ((x:xs):t) = x:xs ++ concat' t

--13

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

--14

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

--15

heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t 
heads' ((h:_):t) = h : heads' t

--16

total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17

fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((x,y,z):t) = (x,z) : fun' t

--18

cola' :: [(String,b,c)] -> String
cola' [] = ""
cola' ((s,_,_):t) = s ++ cola' t

--19

idade' :: Int -> Int -> [(String,Int)] -> [String]
idade' _ _ [] = []
idade' a i ((nome,datan):t)
                        | datan + i >= a = nome : idade' a i t 
                        | otherwise = idade' a i t

