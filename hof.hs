mulThree :: (Num a) => a -> a -> a -> a
mulThree x y z = x * y * z

mulTwoWithNine :: (Num a) => a -> a -> a
mulTwoWithNine = mulThree 9

mulOneWithEighteen :: (Num a) => a -> a
mulOneWithEighteen = mulThree 2 9

cmpWith100 :: (Num a, Ord a) => a -> Ordering
cmpWith100 x = compare 100 x

cmpWith100' :: (Num a, Ord a) => a -> Ordering
cmpWith100' = compare 1000

divByTen :: (Floating a) => a -> a
divByTen = (/ 10)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A' .. 'Z'])

inc :: (Num a) => a -> a
inc = (+ 1)

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x = x : filter' p xs
    | otherwise = filter' p xs

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n: collatz (n `div` 2)
    | odd n = n: collatz (3 * n + 1)

numLongCollatz :: Int
numLongCollatz = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

numLongCollatz' :: (Integral a) => Int -> [a] -> Int
numLongCollatz' x xs = length (filter (\ys -> length ys > x) (map collatz xs))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThreeLambda :: (Num a) => a -> a-> a-> a
addThreeLambda = \x -> \y -> \z -> x + y + z

addThreeCollapsedLambda :: (Num a) => a -> a-> a-> a
addThreeCollapsedLambda = \x y z -> x + y + z
