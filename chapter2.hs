
-- HC2T1 - Task 1: Checking Types in GHCi -- Expected Types: -- 42 :: Int -- 3.14 :: Double -- "Haskell" :: String -- 'Z' :: Char -- True && False :: Bool

-- Type check in GHCi: -- :t 42 -- :t 3.14 -- :t "Haskell" -- :t 'Z' -- :t True && False

-- HC2T2 - Task 2: Function Type Signatures and Implementations

add :: Int -> Int -> Int add x y = x + y

isEven :: Int -> Bool isEven n = n mod 2 == 0

concatStrings :: String -> String -> String concatStrings s1 s2 = s1 ++ s2

-- HC2T3 - Task 3: Immutable Variables

myAge :: Int myAge = 20

piValue :: Double piValue = 3.14159

greeting :: String greeting = "Hello, Haskell!"

isHaskellFun :: Bool isHaskellFun = True

-- Uncommenting below will cause an error due to immutability: -- myAge = 25

-- HC2T4 - Task 4: Infix and Prefix Notations

-- Infix to Prefix infixToPrefix1 = (+) 5 3 infixToPrefix2 = (*) 10 4 infixToPrefix3 = (&&) True False

-- Prefix to Infix prefixToInfix1 = 7 + 2 prefixToInfix2 = 6 * 5 prefixToInfix3 = True && False

-- HC2T5 - Task 5: Defining and Using Functions

circleArea :: Float -> Float circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int maxOfThree a b c = max a (max b c)

-- HC2T6 - Task 6: Understanding Int vs Integer

smallNumber :: Int smallNumber = 262

bigNumber :: Integer bigNumber = 2127

-- Check in GHCi: -- 2^64 :: Int

-- HC2T7 - Task 7: Boolean Expressions

expr1 :: Bool expr1 = True && True -- True

expr2 :: Bool expr2 = False || False -- False

expr3 :: Bool expr3 = not False -- True

expr4 :: Bool expr4 = 10 < 5 -- False

-- You can test by loading this file into GHCi: -- > :load HC2Tasks.hs -- > add 3 4 -- > isEven 10 -- > concatStrings "Hi, " "there!" -- > circleArea 5 -- > maxOfThree 7 3 10
