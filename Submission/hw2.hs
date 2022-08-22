module HW2 where

-- EX1 --
sieve :: Int -> [(Int, Bool)]
sieve 0 = []
sieve n =
  let startingValues = sieveResets n
   in sieveRun startingValues

sieveResets :: Int -> [(Int, Bool)]
sieveResets n = (1, False) : zip [2 .. n] [True | x <- [2 .. n]]

sieveRun :: [(Int, Bool)] -> [(Int, Bool)]
sieveRun [] = []
sieveRun (x : xs) =
  let current_num = fst x
      is_prime = snd x
   in if is_prime
        then x : sieveRun (sieveEpoch xs current_num)
        else x : sieveRun xs

sieveEpoch :: [(Int, Bool)] -> Int -> [(Int, Bool)]
sieveEpoch [] _ = []
sieveEpoch (x : xs) currentPrime =
  let current_num = fst x
   in if current_num `mod` currentPrime == 0
        then (current_num, False) : sieveEpoch xs currentPrime
        else x : sieveEpoch xs currentPrime

-- EX 2 --
data IntList = Single Int | Multi [IntList] deriving (Show)

sum' :: IntList -> Int
sum' (Single n) = n
sum' (Multi newlist) = sum (map sum' newlist)

flatten :: IntList -> [Int]
flatten (Single n) = [n]
flatten (Multi newlist) = concatMap flatten newlist

-- EX 3 --
data BinTree = Empty | Leaf Int | Node BinTree Int BinTree deriving (Show)

make_balanced_tree :: [Int] -> BinTree
make_balanced_tree [] = Empty
make_balanced_tree [root] = Leaf root
make_balanced_tree list =
  let middleIndex = (length list) `div` 2
      ledtLeaf = take middleIndex list
      rightLeaf = drop (middleIndex + 1) list
      middle = extractIndex list middleIndex
   in Node (make_balanced_tree ledtLeaf) middle (make_balanced_tree rightLeaf)

extractIndex :: [Int] -> Int -> Int
extractIndex list i = head (drop i (take (i + 1) list))

add_item :: Int -> BinTree -> BinTree
add_item value Empty = Node (make_balanced_tree []) value (make_balanced_tree [])
add_item value tree =
  let currentList = extractOrderNewList tree
      newList = insertVal currentList value
   in make_balanced_tree newList

extractOrderNewList :: BinTree -> [Int]
extractOrderNewList Empty = []
extractOrderNewList (Leaf a) = [a]
extractOrderNewList (Node leftTree a rightTree) = extractOrderNewList leftTree ++ [a] ++ extractOrderNewList rightTree

insertVal :: [Int] -> Int -> [Int]
insertVal [] a = [a]
insertVal (x : xs) a =
  if x > a
    then a : x : xs
    else x : insertVal xs a

-- Example for EX4 --
f :: String -> Either String String
f item = if (odd (length item)) then Left item else Right item

-- EX4 --
split_by_either :: [t] -> (t -> Either t t) -> ([t], [t])
split_by_either [] f = ([], [])
split_by_either (x : xs) f =
  let res = f x
      restOfList = split_by_either xs f
   in if thisIsLeft res
        then (x : (fst restOfList), snd restOfList)
        else (fst restOfList, x : (snd restOfList))

thisIsLeft :: Either t t -> Bool
thisIsLeft (Left a) = True
thisIsLeft (Right a) = False

--  Example for EX5 --
addOp :: Float -> Float -> Maybe Float
addOp a b = Just (a + b)

divOp :: Float -> Float -> Maybe Float
divOp a b = if (b == 0) then Nothing else Just (a / b)

-- EX5 --
type BinOp = Float -> Float -> Maybe Float

data ExprTree = ExprValue Float | ExprNode ExprTree BinOp ExprTree

eval :: ExprTree -> Maybe Float
eval (ExprValue a) = Just a
eval (ExprNode leftEx f rightEx) = testBinOp f (eval leftEx) (eval rightEx)

testBinOp :: BinOp -> Maybe Float -> Maybe Float -> Maybe Float
testBinOp f (Just a) (Just b) = f a b
testBinOp f _ _ = Nothing
