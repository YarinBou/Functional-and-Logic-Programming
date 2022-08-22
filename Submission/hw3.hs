{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use isNothing" #-}

module HW3 where

-- Q1
type Matrix t = [[t]]

-- Q1.a
is_square :: Matrix t -> Bool
is_square [[]] = True
is_square x = all (\y -> length y == length x) x

-- Q1.b
map_matrix :: (t -> t) -> Matrix t -> Matrix t
map_matrix func = map (func_on_row func)

func_on_row :: (t -> t) -> [t] -> [t]
func_on_row = map

-- Q1.c
map_matrix2 :: (t -> (Int, Int) -> u) -> Matrix t -> Matrix u
map_matrix2 func matrix = map_matrix2_helper func matrix 0

map_matrix2_helper :: (t -> (Int, Int) -> u) -> Matrix t -> Int -> Matrix u
map_matrix2_helper func [x] row = [map_matrix2_row func x (row, 0)]
map_matrix2_helper func (x : xs) row = map_matrix2_row func x (row, 0) : map_matrix2_helper func xs (row + 1)

map_matrix2_row :: (t -> (Int, Int) -> u) -> [t] -> (Int, Int) -> [u]
map_matrix2_row _ [] _ = []
map_matrix2_row f (x : xs) indices = f x indices : map_matrix2_row f xs (fst indices, snd indices + 1)

--question 1.d
transpose :: Matrix t -> Matrix t
transpose matrix = map_matrix2 (\_ (i, j) -> matrix !! j !! i) matrix

--Q2
data BinTree = Empty | Leaf Int | Node BinTree Int BinTree deriving (Show, Eq)

--Q2.a
to_string :: BinTree -> String
to_string Empty = ""
to_string (Leaf x) = show x
to_string (Node left x right) = show x ++ "(" ++ to_string left ++ "," ++ to_string right ++ ")"

--Q2.b
data BinTreeTok = LPar | RPar | Comma | Value Int deriving (Show, Eq)

tokenize :: String -> Maybe [BinTreeTok]
tokenize "" = Nothing
tokenize str =
  let spaceFiltere = filter (/= ' ') str
   in let tokenMap = map tokenizeMap spaceFiltere
       in if Nothing `elem` tokenMap
            then Nothing
            else Just (collectDigits (map removeJust tokenMap))

collectDigits :: [BinTreeTok] -> [BinTreeTok]
collectDigits [] = []
collectDigits ((Value x) : (Value y) : xs) = collectDigits (Value (x * 10 + y) : xs)
collectDigits (x : xs) = x : collectDigits xs

-- map string to token data type
tokenizeMap :: Char -> Maybe BinTreeTok
tokenizeMap x
  | x == '(' = Just LPar
  | x == ')' = Just RPar
  | x == ',' = Just Comma
  | x `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] = Just (Value (read [x] :: Int))
  | otherwise = Nothing

removeJust :: Maybe BinTreeTok -> BinTreeTok
removeJust Nothing = Value (-1)
removeJust (Just x) = x

--Q2.c

compile :: String -> BinTree
compile "" = Empty
compile repStr =
  let treeToken = tokenize repStr
   in if treeToken == Nothing
        then Empty
        else deTokenize (justRemove treeToken)

justRemove :: Maybe [BinTreeTok] -> [BinTreeTok]
justRemove Nothing = []
justRemove (Just x) = x

lParCounter :: BinTreeTok -> Int -> Int
lParCounter LPar num = num + 1
lParCounter _ num = num

commaTokenCounter :: BinTreeTok -> Int -> Int
commaTokenCounter Comma num = num + 1
commaTokenCounter _ num = num

findSeperatingComma :: [BinTreeTok] -> Int -> Int -> Int -> Int
findSeperatingComma [] commaIdx commaAcc lPAcc = commaIdx
findSeperatingComma ((Value x) : xs) commaIdx commaAcc lPAcc = findSeperatingComma xs (commaIdx + 1) commaAcc lPAcc
findSeperatingComma (x : xs) commaIdx commaAcc lPAcc =
  let lpCount = lParCounter x lPAcc
   in let commaCount = commaTokenCounter x commaAcc
       in if commaCount == lpCount
            then commaIdx
            else findSeperatingComma xs (commaIdx + 1) commaCount lpCount

deTokenize :: [BinTreeTok] -> BinTree
deTokenize [] = Empty
deTokenize [Value x] = Leaf x
deTokenize [Comma, Value x] = Leaf x
deTokenize [LPar, Value x, Comma] = Leaf x
deTokenize [Value x, Comma] = Leaf x
deTokenize [Comma, Value x, RPar] = Leaf x
deTokenize [LPar, Comma] = Empty
deTokenize [Comma, RPar] = Empty
deTokenize [LPar, Comma, RPar] = Empty
deTokenize (value : ifCommaValue : ls) =
  let list = if value == Comma then ls else ifCommaValue : ls
   in let val = if value == Comma then ifCommaValue else value
       in let commaIdx = findSeperatingComma list 0 0 0
           in let startList = take (commaIdx + 1) list
               in let endList = drop commaIdx list
                   in Node (deTokenize (drop 1 startList)) (extractValueToken val) (deTokenize (take (length endList - 1) endList))
deTokenize _ = Empty

extractValueToken :: BinTreeTok -> Int
extractValueToken (Value x) = x
extractValueToken _ = -1