double x = x * 2

doubleSmallNumber x =
    if x >= 100
    then x
    else x * 2

-- rightAngledTriangles generates all right angled triangles with lengths from 1 to 10-
rightAngledTriangles = [(a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]

-- niners returns the first 9 numbers which are divisible by 99--
niners = take 9 [x | x <- [1..], mod x 99 == 0]
