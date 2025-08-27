-- Save as Main.hs
module Main where

-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value   = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount =
  "Transaction: from " ++ from ++ " to " ++ to ++ " with value " ++ show amount

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency
  deriving Show

data Person = Person String (String, Int) PaymentMethod
  deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 42) Cash

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float
  deriving Show

area :: Shape -> Float
area (Circle r)        = pi * r * r
area (Rectangle w h)   = w * h

circleExample :: Shape
circleExample = Circle 5

rectExample :: Shape
rectExample = Rectangle 10 5

-- HC8T4: Record Syntax for Employee
data Employee = Employee
  { empName :: String
  , experienceInYears :: Float
  } deriving Show

richard :: Employee
richard = Employee { empName = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record Syntax for Person
data PersonRec = PersonRec
  { personName :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving Show

person1 :: PersonRec
person1 = PersonRec { personName = "Alice", age = 30, isEmployed = True }

person2 :: PersonRec
person2 = PersonRec { personName = "Eve", age = 25, isEmployed = False }

-- Main function demo
main :: IO ()
main = do
  putStrLn "=== HC8 Tasks Demo ==="

  -- HC8T1
  putStrLn $ generateTx "addr1" "addr2" 100

  -- HC8T2
  putStrLn $ "Person bob: " ++ show bob

  -- HC8T3
  putStrLn $ "Area of Circle (r=5): " ++ show (area circleExample)
  putStrLn $ "Area of Rectangle (10x5): " ++ show (area rectExample)

  -- HC8T4
  putStrLn $ "Employee Richard: " ++ show richard

  -- HC8T5
  putStrLn $ "Person1 (employed): " ++ show person1
  putStrLn $ "Person2 (unemployed): " ++ show person2
