module Main where

-- HC11T1: Greet the User
greetUser :: String -> String
greetUser name = "Hello, " ++ name ++ "!"

-- HC11T2: Count Characters in a Line
countChars :: String -> Int
countChars line = length line

-- HC11T3: Double a Number
doubleNumber :: Int -> Int
doubleNumber num = num * 2

-- HC11T4: Concatenate Two Lines
concatLines :: String -> String -> String
concatLines l1 l2 = l1 ++ l2

-- HC11T5: Repeat Until "quit"
repeatUntilQuit :: [String] -> [String]
repeatUntilQuit [] = []
repeatUntilQuit (x:xs)
    | x == "quit" = ["Goodbye!"]
    | otherwise   = ("You entered: " ++ x) : repeatUntilQuit xs

main :: IO ()
main = do
    putStrLn "HC11 Non-Interactive Demo"
    print (greetUser "Elkanah")
    print (countChars "Hello World")
    print (doubleNumber 7)
    print (concatLines "foo" "bar")
    print (repeatUntilQuit ["hi", "yes", "quit", "ignored"])
