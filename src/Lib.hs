module Lib where

import qualified Data.Text as Text

someFunc :: IO ()
someFunc = print (compress "ssssalllmmmmaaaaa")

data IdhuOrAdhu a b = Idathu a | Valadhu b deriving (Show)

myLast :: [a] -> IdhuOrAdhu String a
myLast [] = Idathu "No end for empty list"
myLast [x] = Valadhu x
myLast (_:xs) = myLast xs

printIdhuOrAdhu :: (Show a) => IdhuOrAdhu String a -> IO()
printIdhuOrAdhu x =
  case x of
    Idathu a -> print a
    Valadhu b -> print b

myLastButOne :: [a] -> IdhuOrAdhu String a
myLastButOne [] = Idathu "No last but one element for empty list"
myLastButOne [_] = Idathu "No last but one element for a single element in a list"
myLastButOne [x,_] = Valadhu x
myLastButOne (_:xs) = myLastButOne xs

nthElement :: [a] -> Int -> IdhuOrAdhu String a
nthElement [] _ = Idathu "cant find the nth element for empty list"
nthElement (x:_) 1 = Valadhu x
nthElement _ 0 = Idathu "Cant find 0th element in a list"
nthElement list'@(_:xs) n = do
  let length' = lnth list'
  if length' < n
    then Idathu "n is greater than the length of the list"
    else nthElement xs (n-1)

lnth :: [a] -> Int
lnth [] = 0
lnth [_] = 1
lnth (_:xs) = 1 + lnth xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' [x,y] = [y,x]
reverse' (x:xs) = reverse' xs ++ [x]

compress :: [Char] -> [Char]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x==y then compress (y:xs) else x: compress (y:xs)

pluralize :: Text.Text -> Text.Text
pluralize list
  | Text.null list = Text.pack "No plural for an empty string"
  | Text.last list == 's' = list
  | Text.last list /= 's' =list <> Text.pack "s"
  | otherwise = Text.pack "undefined"


