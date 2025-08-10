import Data.Char (isUpper)

-- HC5T1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))
-- Example: applyThrice (+1) 2 ==> 5

-- HC5T2: Filter odd numbers from 1 to 30
oddNumbers :: [Int]
oddNumbers = filter odd [1..30]
-- Output: [1,3,5,7,9,11,13,15,17,19,21,23,25,27,29]

-- HC5T3: Check if any word starts with an uppercase letter
startsWithUppercase :: [String] -> Bool
startsWithUppercase = any (\w -> not (null w) && isUpper (head w))
-- Example: startsWithUppercase ["hello", "World"] ==> True

-- HC5T4: Lambda version of biggerThan10
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10
-- Example: biggerThan10 12 ==> True

-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5
-- Example: multiplyByFive 6 ==> 30

-- HC5T6: Function composition to square and filter even results
evenSquares :: [Int] -> [Int]
evenSquares = filter even . map (^2)
-- Example: evenSquares [1..6] ==> [4, 16, 36]

-- HC5T7: Rewrite using $
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]
-- Output: 2*(4+5+6+7+8+9+10) = 2*49 = 98

-- HC5T8: Point-free version of addFive
addFive :: Int -> Int
addFive = (+ 5)
-- Example: addFive 10 ==> 15

-- HC5T9: Apply a function twice to each element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)
-- Example: transformList (+1) [1,2,3] ==> [3,4,5]

-- HC5T10: Check if any square is greater than 50
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2)
-- Example: anySquareGreaterThan50 [1..10] ==> True

-- Main function for demo/testing
main :: IO ()
main = do
    print $ applyThrice (*2) 1                -- 8
    print oddNumbers                          -- [1,3,5,...,29]
    print $ startsWithUppercase ["hello", "World"] -- True
    print $ biggerThan10 11                   -- True
    print $ multiplyByFive 7                  -- 35
    print $ evenSquares [1..6]                -- [4,16,36]
    print result                              -- 98
    print $ addFive 5                         -- 10
    print $ transformList (*2) [1,2,3]        -- [4,8,12]
    print $ anySquareGreaterThan50 [1..7]     -- False
    print $ anySquareGreaterThan50 [1..8]     -- True
