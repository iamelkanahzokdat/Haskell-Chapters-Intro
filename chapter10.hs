module Main where

-- HC10T1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Cryptocurrency
  deriving Show

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash"
  showSimple Card = "Card"
  showSimple Cryptocurrency = "Crypto"

-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable Type Class
data Blockchain = Bitcoin | Ethereum | Cardano
  deriving Show

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith Bitcoin Ethereum = LT
  compareWith Ethereum Bitcoin = GT
  compareWith b1 b2
    | b1 == b2  = EQ
    | otherwise = LT

-- HC10T4: Eq Instance for Box
data Box a = Empty | Has a
  deriving Show

instance Eq a => Eq (Box a) where
  Empty == Empty = True
  Has x == Has y = x == y
  _ == _ = False

-- HC10T5: ShowDetailed Type Class
data User = User { userName :: String, userAge :: Int } deriving Show

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed u = "User: " ++ userName u ++ ", Age: " ++ show (userAge u)

-- HC10T6: Fixed Eq for Blockchain (safe)
instance Eq Blockchain where
  Bitcoin == Bitcoin = True
  Ethereum == Ethereum = True
  Cardano == Cardano = True
  _ == _ = False

  x /= y = not (x == y)

-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert = showSimple

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq Blockchain where
  compareEquality x y = x == y

-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

-- Demo Main Function
main :: IO ()
main = do
  putStrLn "=== HC10 Tasks Demo ==="

  -- T1
  putStrLn $ "ShowSimple Card: " ++ showSimple Card

  -- T2 (type annotation for [Int])
  putStrLn $ "SumUp [1..5]: " ++ show (sumUp ([1..5] :: [Int]))

  -- T3
  putStrLn $ "Compare Bitcoin vs Ethereum: " ++ show (compareWith Bitcoin Ethereum)

  -- T4
  putStrLn $ "Boxes equal? " ++ show (Has 5 == Has (5 :: Int))

  -- T5
  let user = User "Alice" 30
  putStrLn $ showDetailed user

  -- T6
  putStrLn $ "Blockchain eq test: " ++ show (Bitcoin == Bitcoin)
  putStrLn $ "Blockchain neq test: " ++ show (Bitcoin /= Ethereum)

  -- T7
  putStrLn $ "Convert Cash to String: " ++ convert Cash

  -- T8
  putStrLn $ "AdvancedEq compare Bitcoin/Ethereum: " ++ show (compareEquality Bitcoin Bitcoin)

  -- T9 (type annotation for Int)
  putStrLn $ "Min/Max Int: " ++ show (minValue :: Int) ++ ", " ++ show (maxValue :: Int)

  -- T10
  putStrLn $ "Concatenatable Strings: " ++ concatWith "Hello, " "World!"
