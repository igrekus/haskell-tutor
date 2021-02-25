import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques xs = length (nub xs)

numUniques' :: (Eq c) => [c] -> Int
numUniques' = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haysack = 
    let nlen = length needle
    in foldl (\acc x -> (take nlen x == needle) || acc) False (tails haysack)

encode :: Int -> String -> String 
encode shift message = 
    let ords = map ord message
        shifted = map (+ shift) ords
    in map chr shifted

encode' :: Int -> String -> String 
encode' shift = 
    map (chr . (+ shift) . ord)

decode :: Int -> String -> String
decode shift = encode (negate shift)

