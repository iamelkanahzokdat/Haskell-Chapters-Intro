-- HC6T1: Factorial (Recursive)
-- Computes n! using recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n
    | n > 0     = n * factorial (n - 1)
    | otherwise = error "Factorial is not defined for negative numbers"

-- HC6T2: Fibonacci (Recursive)
-- Computes the nth Fibonacci number using recursion
fibonacci :: (Integral a) => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n
    | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
    | otherwise = error "Fibonacci is not defined for negative indices"

-- HC6T3: Sum of Elements Using foldr
sumUsingFoldr :: (Num a) => [a] -> a
sumUsingFoldr = foldr (+) 0

-- HC6T4: Product of Elements Using foldl
productUsingFoldl :: (Num a) => [a] -> a
productUsingFoldl = foldl (*) 1

-- HC6T5: Reverse a List (Recursive)
reverseList :: [a] -> [a]
reverseList []     = []
reverseList (x:xs) = reverseList xs ++ [x]

-- HC6T6: Element Exists in List
elementExists :: (Eq a) => a -> [a] -> Bool
elementExists _ [] = False
elementExists e (x:xs)
    | e == x    = True
    | otherwise = elementExists e xs

-- HC6T7: List Length
listLength :: [a] -> Int
listLength []     = 0
listLength (_:xs) = 1 + listLength xs

-- HC6T8: Filter Even Numbers
filterEvenNumbers :: (Integral a) => [a] -> [a]
filterEvenNumbers = filter even

-- HC6T9: Map Implementation
mapCustom :: (a -> b) -> [a] -> [b]
mapCustom _ []     = []
mapCustom f (x:xs) = f x : mapCustom f xs

-- HC6T10: Digits of a Number (Recursive)
digitsOfNumber :: (Integral a) => a -> [a]
digitsOfNumber n
    | n < 0     = error "Only works with non-negative numbers"
    | n < 10    = [n]
    | otherwise = digitsOfNumber (n `div` 10) ++ [n `mod` 10]

-- MAIN FUNCTION (Optional for testing all tasks)
main :: IO ()
main = do
    putStrLn "Factorial of 5:"
    print (factorial 5)

    putStrLn "Fibonacci of 6:"
    print (fibonacci 6)

    putStrLn "Sum using foldr:"
    print (sumUsingFoldr [1, 2, 3, 4, 5])

    putStrLn "Product using foldl:"
    print (productUsingFoldl [1, 2, 3, 4, 5])

    putStrLn "Reverse a list:"
    print (reverseList [1, 2, 3, 4, 5])

    putStrLn "Element exists (3 in [1,2,3,4,5]):"
    print (elementExists 3 [1, 2, 3, 4, 5])

    putStrLn "Length of list [1,2,3,4,5]:"
    print (listLength [1, 2, 3, 4, 5])

    putStrLn "Filter even numbers from [1..10]:"
    print (filterEvenNumbers [1..10])

    putStrLn "Map custom (*2) on [1..5]:"
    print (mapCustom (*2) [1..5])

    putStrLn "Digits of 12345:"
    print (digitsOfNumber 12345)
