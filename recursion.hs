maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
            | x > maxTail = x
            | otherwise = maxTail
            where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "empty list"
maximum'' [x] = x
maximum'' (x:xs)
    | x > maximum'' xs = x
    | otherwise = maximum'' xs

maximum''' :: (Ord a) => [a] -> a
maximum''' [] = error "empty list"
maximum''' [x] = x
maximum''' (x:xs) = max x (maximum''' xs)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
