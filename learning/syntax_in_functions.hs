-- lucky is a function that uses pattern matching and fall throughs
-- in other words, lucky 7 must come before lucky x
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- factorial function using pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Barry"
charName 'c' = "Charlie"
charName x = "Unknown"

addVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- functions that extracts first, second and third from triples (like fst and snd of tuples)
fst_t :: (a, b, c) -> a
fst_t (x, _, _) = x

snd_t :: (a, b, c) -> b
snd_t (_, y, _) = y

trd_t :: (a, b, c) -> c
trd_t (_, _, z) = z

addPairs :: Num a => [(a, a)] -> [a]
addPairs xs = [a + b | (a, b) <- xs]

-- own implementation of the head function
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

-- trivial function that tells us some of the first elems in a list
-- the function is safe because it takes care of the empty list, a singleton list,
-- a list with two elements, and a list with more than two elements
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- own implementation of the length function
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- own implementation of the sum function
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- There's also a thing called as patterns.
-- Those are a handy way of breaking something up
-- according to a pattern and binding it to names
-- whilst still keeping a reference to the whole thing.
-- You do that by putting a name and an @ in front of a pattern.
-- For instance, the pattern xs@(x:y:ys).
-- This pattern will match exactly the same thing as x:y:ys
-- but you can easily get the whole list via xs
-- instead of repeating yourself by typing out x:y:ys
-- in the function body again.
capital :: String -> String
capital "" = "Empty string"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards; note that there's no = after the fn name and params
heightTell :: (RealFloat a) => a -> String
heightTell height
    | height <= 155 = "You're very short"
    | height <= 174 = "You're short"
    | height <= 182 = "You're pretty tall"
    | otherwise = "You're really tall"

-- own implementation of the compare function
compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a < b = LT
    | a > b = GT
    | otherwise = EQ

-- DRY in guards
-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--     | weight / height ^ 2 <= 18.5 = "Underweight"
--     | weight / height ^ 2 <= 25.0 = "Normal"
--     | weight / height ^ 2 <= 30.0 = "Overweight"
--     | otherwise = "Unknown"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "Underweight"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Overweight"
    | otherwise = "Unknown"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- initials func, using where
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

-- initials func, using pattern matching in params
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

-- function that takes a list of weight-height pairs and returns a list of BMIs
-- note that there's a function 'bmi' that is defined in the where claue
calcBMIs :: (RealFloat a) => [(a, a)] -> [a]
calcBMIs xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let bindings in functions
cylinder :: (RealFloat a) => a -> a -> a
cylinder radius height =
    let sideArea = 2 * pi * radius * height
        topArea = pi * radius ^ 2
    in sideArea + 2 * topArea

-- you can also have let bindings in expressions
-- 4 * (let a = 9 in a + 1) + 2 evaluates to 42


-- if let bindings are so cool,
-- why not use them all the time instead of where bindings?
--
-- since let bindings are expressions and are fairly local
-- in their scope, they can't be used across guards.
--
-- some people prefer where bindings because the names come after
-- the function they're being used in.
--
-- that way, the function body is closer to its name and type declaration
-- and to some that's more readable.


-- case expressions also exist
--
-- instead of
-- head' :: [a] -> a
-- head' [] = error "No head for empty lists"
-- head' (x:_) = x
--
-- we can have:
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists"
(x:_) -> x
