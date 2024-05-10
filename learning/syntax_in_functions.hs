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
