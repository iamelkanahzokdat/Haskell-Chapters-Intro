#haskell-chapter-4
-- HC4T1 - weatherReport
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

-- HC4T2 - dayType
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
  | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
  | otherwise = "Invalid day"

-- HC4T3 - gradeComment
gradeComment :: Int -> String
gradeComment grade
  | grade >= 90 && grade <= 100 = "Excellent!"
  | grade >= 70 && grade <= 89  = "Good job!"
  | grade >= 50 && grade <= 69  = "You passed."
  | grade >= 0  && grade <= 49  = "Better luck next time."
  | otherwise = "Invalid grade"

-- HC4T4 & HC4T5 - specialBirthday using pattern matching with catch-all
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! So cute!"
specialBirthday 18 = "Adulting begins!"
specialBirthday 50 = "Half a century old!"
specialBirthday age = "You are " ++ show age ++ " years old. Happy Birthday!"

-- HC4T6 - whatsInsideThisList
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one item."
whatsInsideThisList [_, _]   = "The list has two items."
whatsInsideThisList _        = "The list has many items."

-- HC4T7 - firstAndThird
firstAndThird :: [a] -> [a]
firstAndThird (x:_:z:_) = [x, z]
firstAndThird _         = []

-- HC4T8 - describeTuple
describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

-- Optional main function to test
main :: IO ()
main = do
  putStrLn "=== HC4 Tasks Demo ==="
  putStrLn $ "weatherReport 'sunny': " ++ weatherReport "sunny"
  putStrLn $ "dayType 'Monday': " ++ dayType "Monday"
  putStrLn $ "gradeComment 85: " ++ gradeComment 85
  putStrLn $ "specialBirthday 21: " ++ specialBirthday 21
  putStrLn $ "whatsInsideThisList [1,2]: " ++ whatsInsideThisList [1,2]
  putStrLn $ "firstAndThird [10,20,30,40]: " ++ show (firstAndThird [10,20,30,40])
  putStrLn $ "describeTuple (\"Alice\", 30): " ++ describeTuple ("Alice", 30)
