--HASKELL Playground 

doubleMe x = x * 2

doubleUs  x y = doubleMe x + doubleMe y

-- example of if statement
doubleSmallerNumber x = if x > 10
    then x
    else doubleMe x

-- sleeker if branch
doubleSmallerNumber' x = (if x > 100 then x else doubleMe x) + 1

-- example definition (function with no parameters)
conanO'Brien = "I'm a boring string"

-- functions with types
factorial :: Integer -> Integer 
factorial n = product [1..n]

circumference :: Float -> Float 
circumference r = 2 * pi * r

-- pattern matching 
sayNum :: (Integral a) => a -> String
sayNum 1 = "One"
sayNum 2 = "Two"
sayNum 3 = "Three"
sayNum x = "Nope"

-- pattern matching 2
first :: (a, b, c) -> a
first (x, _, _) = x

-- pattern matching 3
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

-- pattern matching 4 and recursion 
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- recursion 
factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n -1)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- guards 
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)  


-- let bindings
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

-- switch case 
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


-- $ replaces opening and closing Parentheses 
    -- sum (filter (> 10) (map (*2) [2..10]))
    -- sum $ filter (> 10) $ map (*2) [2..10]

-- . function composition
    -- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 
    -- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

-- custom types 
data Bool = True | False 

-- custom types 2
--           Name   Parameters                                            derive class 
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- custom types 3
-- Car {company = "Ford", model = "Mustang", year = 1967} 
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- custom types 4
data Vector v = Vector v v v deriving (Show)

-- custom types in functions
-- v1 `vplus` v2
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  

-- surface $ Circle 10 20 10 
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
