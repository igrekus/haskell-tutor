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
