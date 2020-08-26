doubler x = x + x

twoDoubler x y = doubler x + doubler y

doubleSmall x = if x > 100
                then x
                else doubler x

boombangs xs = [if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x]

length' xs = sum [1 | x <- xs]

triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

lucky :: (Integral a) => a -> String
lucky 7 = "lucky seven"
lucky x = "not lucky"

fac :: (Integral a) => a -> a
fac 0 = 1
fac x = x * fac (x - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVecs :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVecs a b = (fst a + fst b, snd a + snd b)

addVecsPatt :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVecsPatt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "no"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "empty"
tell (x:[]) = "one element " ++ show x
tell (x:y:[]) = "two elements " ++ show x ++ " " ++ show y
tell (x:y:_) = "long list, two elems: " ++ show x ++ " " ++ show y 

len :: (Num b) => [a] -> b
len [] = 0
len (_:xs) = 1 + len xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "empty string"
capital all@(x:xs) = "first letter of " ++ show all ++ " is " ++ show x

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "fat"
    | otherwise = "whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "underweight"
    | weight / height ^ 2 <= 25.0 = "normal"
    | weight / height ^ 2 <= 30.0 = "fat"
    | otherwise = "whale"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise = "whale"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylArea :: (RealFloat a) => a -> a -> a
cylArea r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

head'' :: [a] -> a
head'' xs = case xs of [] -> error "empty"
                       (x:_) -> x
