module Main where

-- HC7T1: Eq instance for Color
data Color = Red | Green | Blue deriving (Show, Enum, Bounded)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

-- HC7T2: Ord instance for Color (Red < Green < Blue)
instance Ord Color where
    compare Red Green  = LT
    compare Red Blue   = LT
    compare Green Blue = LT
    compare Green Red  = GT
    compare Blue Red   = GT
    compare Blue Green = GT
    compare _ _        = EQ

-- HC7T3: compareValues with Eq + Ord constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double
    deriving (Read, Show)

-- Eq + Ord instances for Shape
instance Eq Shape where
    (Circle r1) == (Circle r2)               = r1 == r2
    (Rectangle w1 h1) == (Rectangle w2 h2)   = w1 == w2 && h1 == h2
    _ == _                                   = False

instance Ord Shape where
    compare (Circle r1) (Circle r2)             = compare r1 r2
    compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2) -- compare by area
    compare (Circle _) (Rectangle _ _)          = LT   -- Circles < Rectangles
    compare (Rectangle _ _) (Circle _)          = GT

-- HC7T5: Function with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: circleCircumference
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

-- HC7T7: Bounded + Enum
nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise     = succ c

-- HC7T8: Parse a Shape from String
parseShape :: String -> Maybe Shape
parseShape str = case reads str of
    [(s, "")] -> Just s
    _         -> Nothing

-- HC7T9: Describable class
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "This is True"
    describe False = "This is False"

instance Describable Shape where
    describe (Circle r)       = "A circle with radius " ++ show r
    describe (Rectangle w h)  = "A rectangle " ++ show w ++ " by " ++ show h

-- HC7T10: Multiple constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y =
    if x >= y then describe x else describe y

-- Demo
main :: IO ()
main = do
    putStrLn "HC7 Demo Results"
    print (Red == Red, Red == Blue)                        -- Eq instance
    print (compare Green Blue, compare Red Blue)           -- Ord instance
    print (compareValues 10 20)                            -- Multiple constraints
    print (show (Circle 5), read "Rectangle 3 4" :: Shape) -- Show & Read
    print (squareArea 4 :: Int)                            -- Num constraint
    print (circleCircumference 10 :: Double)               -- Floating constraint
    print (nextColor Blue)                                 -- Enum + Bounded
    print (parseShape "Circle 2")                          -- Parse
    print (describe True, describe (Rectangle 3 6))        -- Describable
    print (describeAndCompare (Circle 5) (Circle 2))       -- Multiple constraints
