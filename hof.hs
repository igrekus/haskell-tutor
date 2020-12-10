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
