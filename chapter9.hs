module Main where

-- HC9T1: Parametric Type Synonym
type Entity a = (a, String)  -- (value, address)

-- Example
entityInt :: Entity Int
entityInt = (42, "123 Main St")

-- HC9T2: Parametric Data Type Box
data Box a = Empty | Has a
  deriving Show

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n Empty     = Empty
addN n (Has val) = Has (val + n)

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract def Empty     = def
extract _   (Has val) = val

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a, radius :: Float }
             | Rectangle { color :: a, width :: Float, height :: Float }
             deriving Show

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving Show

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = Nil | Node a (Sequence a)
  deriving Show

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ Nil = False
elemSeq x (Node val next)
  | x == val  = True
  | otherwise = elemSeq x next

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree | NodeBST a (BST a) (BST a)
  deriving Show

-- Demo main function
main :: IO ()
main = do
  putStrLn "=== HC9 Tasks Demo ==="

  -- HC9T1
  putStrLn $ "Entity Int: " ++ show entityInt

  -- HC9T2 & HC9T3 & HC9T4
  let box1 = Has 10
  let box2 = Empty :: Box Int
  putStrLn $ "Box1 after addN 5: " ++ show (addN 5 box1)
  putStrLn $ "Extract from Box2 (default 0): " ++ show (extract 0 box2)

  -- HC9T5
  let circ = Circle "Red" 5
  let rect = Rectangle "Blue" 10 5
  putStrLn $ "Circle: " ++ show circ
  putStrLn $ "Rectangle: " ++ show rect

  -- HC9T6 & HC9T7
  let comment1 = Tweet "Nice!" 3 []
  let comment2 = Tweet "Awesome!" 2 []
  let mainTweet = Tweet "Hello World" 5 [comment1, comment2]
  putStrLn $ "Total engagement of mainTweet: " ++ show (engagement mainTweet)

  -- HC9T8 & HC9T9
  let seq1 = Node 1 (Node 2 (Node 3 Nil))
  putStrLn $ "Is 2 in seq1? " ++ show (elemSeq 2 seq1)
  putStrLn $ "Is 5 in seq1? " ++ show (elemSeq 5 seq1)

  -- HC9T10
  let bst = NodeBST 10 (NodeBST 5 EmptyTree EmptyTree) (NodeBST 15 EmptyTree EmptyTree)
  putStrLn $ "BST example: " ++ show bst
