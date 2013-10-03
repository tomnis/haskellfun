doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

boomBangs :: (Integral a) => [a] -> [[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUpperCase st = [ c | c <- st, elem c ['A'..'Z']]

substring :: [Char] -> [Char] -> Bool
substring a b
  | length b < length a = False
  | a == take (length a) b = True
  | otherwise = substring a (tail b)
  
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT
  
-- use where clause to pattern match
initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [g] ++ "."
  where (f:_) = firstname
        (g:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (g:_) = [f] ++ "." ++ [g] ++ "."

calcbmis :: (RealFloat a) => [(a,a)] -> [a]
calcbmis xs = [bmi w h | (w, h) <- xs]
  where bmi w h = w / h ^ 2
  
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallersorted = quicksort [a | a <- xs, a <= x]
      biggersorted  = quicksort [a | a <- xs, a > x]
  in smallersorted ++ [x] ++ biggersorted

-- test partial function application
-- multThree :: (Num a) => a -> (a -> (a -> a))
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

applytwice :: (a -> a) -> a -> a
applytwice f x = f (f x)

-- join two lists by applying f to corresponding elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs
  
-- play with collatz
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
  | even n = n : chain (div n 2)
  | odd n = n : chain (n*3 + 1)
  
numlongchains :: Int
numlongchains = length (filter islong (map chain [1..100]))
  where islong xs = length xs > 15
  
sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0
