-- syntax overview

x :: Integer
x = 99

f :: Integer -> Integer
f n = n + 10

g :: Integer -> Integer
g = \c -> c + 20

h :: Integer -> Integer -> Integer
h x y = (x + y) * 2

i :: (Integer -> whatever) -> whatever
i k = k 99

hithere :: a -> a
hithere x = x

byethere :: a -> a -> a
byethere x _ = x
-- byethere _ x = x

data Shape =
 Rectangle Integer Integer
 | Circle Integer
 | Triangle Integer Integer Integer
 deriving Show


pie = 3

-- case expression
perimeter :: Shape -> Integer
perimeter = \s -> case s of
     Rectangle x y -> (x+y) *2
     Circle r -> 2*pie*r
     Triangle a b c -> a + b + c

-- pattern matching
perimeteragain :: Shape -> Integer
perimeteragain (Rectangle x y) = (x + y) * 2
perimeteragain (Circle r) = 2 * pie * r
perimeteragain (Triangle a b c) = a + b + c

data Three a = T a a a deriving (Eq, Show)

mapThree :: (a -> b) -> Three a -> Three b
mapThree f (T x1 x2 x3) = T (f x1) (f x2) (f x3)
