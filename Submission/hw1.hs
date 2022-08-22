module HW1 where

-- Q1
shiftString :: String -> String
shiftString [] = []
shiftString x = tail x ++ take 1 x

-- Q2
interleave :: String -> String -> String
interleave "" "" = ""
interleave "" y = y
interleave x "" = x
interleave x y = head x : head y : interleave (tail x) (tail y)

-- Q3
block_split :: String -> [String]
block_split [] = []
block_split stringToSplit = block_split_acc stringToSplit 1

block_split_acc :: String -> Int -> [String]
block_split_acc stringToSplit n =
  let pre = take n stringToSplit
      suf = drop n stringToSplit
   in if suf == ""
        then [pre]
        else pre : block_split_acc suf (n + 1)

-- Q4
parity :: [Int] -> Int
parity list = (-1) ^ permutation_helper list 0

permutation_helper :: [Int] -> Int -> Int
permutation_helper [] n = n
permutation_helper [x] n = n
permutation_helper (x : xs) n = if is_bigger x (head xs) then permutation_helper xs n + 1 else permutation_helper xs n

is_bigger :: Int -> Int -> Bool
is_bigger x y = x > y

-- Q5
my_sqrt :: Float -> Float
my_sqrt 0 = 0
my_sqrt 1 = 1
my_sqrt x = my_sqrt_helper x x 1 0.000001

--
my_sqrt_helper :: Float -> Float -> Float -> Float -> Float
my_sqrt_helper x a b e =
  if a - b > e
    then do
      let diff = (a + b) / 2
      my_sqrt_helper x diff (x / diff) e
    else a