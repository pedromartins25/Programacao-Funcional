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

--20

powerEnumFrom':: Int -> Int -> [Int]
powerEnumFrom' _ 1 = [1]
powerEnumFrom' x y 
             | y > 1 = powerEnumFrom' x (y-1) ++ [x^(y-1)]
             | otherwise = []

--21

isPrime' :: Int -> Bool
isPrime' n = n >= 2 && primeCheck n 2

primeCheck :: Int -> Int -> Bool
primeCheck n m
    | m * m > n = True
    | mod n m == 0 = False
    | otherwise = primeCheck n (m+1)

--22

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (h:t) (h1:t1)
                 | h == h1 = isPrefixOf' t t1
                 | otherwise = False

--23

--isSuffixOf' :: Eq a => [a] -> [a] -> Bool

--24 DÚVIDA
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' l1 l2
                      | l1 == auxSubs l1 l2 = True
                      | otherwise = False


auxSubs :: Eq a => [a] -> [a] -> [a]
auxSubs _ [] = []
auxSubs [] _ = []
auxSubs (h:t) l 
                 | h `elem` l = h : auxSubs t l 
                 | otherwise = auxSubs t l 

--25

elemIndices' :: Eq a => a -> [a] ->  [Int]
elemIndices' x l = elemIndicesAux x l 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux _ [] _ = []
elemIndicesAux x (h:t) i
              | x == h = i : elemIndicesAux x t (i+1)
              | otherwise = elemIndicesAux x t (i+1)

--26

nub':: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t  
             then nub' t
             else h : nub' t
--27

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t)
              | x == h = t
              | otherwise = h : delete' x t

--28 

barraBarra :: Eq a => [a] -> [a] -> [a]
barraBarra [] _ = []
barraBarra l [] = l
barraBarra (h:t) l@(h1:t1) 
               | h == h1 = barraBarra t t1
               | otherwise = h : barraBarra t l

--29

union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t)
        | h `elem` l = union' l t
        | otherwise = union' (l ++ [h]) t


--30

intersect' :: Eq a => [a] -> [a] -> [a]
intersect'[] _ = []
intersect' (h:t) l
           | h `elem` l = h : intersect' t l
           | otherwise = intersect' t l 

--31

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t)
            | h > x = x : h : t
            | otherwise = h : insert' x t

--32

unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords t

--33

unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

--34

pMaior':: Ord a => [a] -> Int
pMaior' [_] = 0
pMaior' (h:t)
           | h >= (t !! pMaior' t) = 0
           | otherwise = 1 + pMaior' t

--35

lookUp' :: Eq a => a -> [(a,b)] -> Maybe b
lookUp' _ [] = Nothing
lookUp' c ((x,y):t)
            | c == x = Just y
            | otherwise = lookUp' c t

--36

preCrescente':: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' (h:s:t)
                | s >= h = h : preCrescente' (s:t)
                | otherwise = [h]

--37

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert' h (iSort' t) 

--38

menor':: String -> String -> Bool
menor' "" _ = False
menor' _ "" = True
menor (h:t) (h':t')
            | h < h' = True
            | h == h' = menor t t'
            | otherwise = False

--39

elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' e ((a,_):t)
                    | e == a = True
                    | otherwise = elemMSet' e t

--40

converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((a,x):t)
                   | x > 0 = a : converteMSet' ((a,x-1): t)
                   | otherwise = a : converteMSet' t

--41

insereMSet':: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' e [] = [(e,1)] 
insereMSet' e ((a,x):t)
                    | e == a = (a,x+1) : t
                    | otherwise = (a,x) : insereMSet' e t

--42

removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' _ [] = []
removeMSet' e ((a,x):t)
                    | e == a && x > 1 = (a,x-1) : t
                    | e == a && x <= 1 = t
                    | otherwise = (a,x) : removeMSet' e t

--43

constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
converteMSet' (l:ls) = insereMSet' l (constroiMSet' ls)

