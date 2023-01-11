module Test.MySolutions where

import Data.Path
import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, foldl, head, length, tail, (..), (:))
import Data.Foldable (product)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe)
import Test.Examples (allFiles)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven i = even i

countEven :: Array Int -> Int
countEven [] = 0
countEven arr = case head arr of
    Nothing -> 0
    Just v -> if even v
        then 1 + (countEven $ fromMaybe [] $ tail arr)
        else countEven  $ fromMaybe [] $ tail arr

squared :: Array Number -> Array Number
squared [] = []
squared arr = map (\e -> e * e) arr

keepNonNegative :: Array Number -> Array Number
keepNonNegative [] = []
keepNonNegative arr = filter (\e -> e >= 0.0) arr

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\n -> n >= 0.0) <$?> arr

factors' :: Int -> Array (Array Int)
factors' n = filter (\xs -> product xs == n) do
    i <- 1 .. n
    j <- i .. n
    pure [ i, j ] -- could also just return [[i,j]]

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = (length $ factors' n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct left right = do
  a_ <- left
  b_ <- right
  [ [ a_, b_ ] ]

triples :: Int -> Array (Array Int)
triples n = do
    i <- 1 .. n
    j <- i .. n
    k <- j .. n
    guard $ i * i + j * j == k * k
    pure [ i, j, k ]

primeFactors :: Int -> Array Int
primeFactors n = factorize 2 n
  where
  factorize :: Int -> Int -> Array Int
  factorize _ 1 = []
  factorize divisor dividend =
    if dividend `mod` divisor == 0 then
      cons divisor $ factorize (divisor) (dividend / divisor)
    else
      factorize (divisor + 1) dividend

allTrue :: Array Boolean -> Boolean
allTrue xs = foldl (\acc e -> e == acc) true xs

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fib' n 2 0 1
    where
    fib' :: Int -> Int -> Int -> Int -> Int
    fib' limit count n1 n2 =
        if limit == count then
            n1 + n2
        else
            fib' limit (count + 1) n2 (n1 + n2)

reverse :: forall a. Array a -> Array a
reverse xs = foldl (\acc e -> [e] <> acc) [] xs

onlyFiles :: Path -> Array Path
onlyFiles path = filter (not isDirectory) (allFiles path)

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ do
    path' <- allFiles path
    child <- ls path'
    guard $ filename child == filename path' <> fileName
    pure path'

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path)
    where
        loop :: Array Path -> Path -> Array Path
        loop [largest, smallest] current
            | size current < size smallest = [largest, current]
            | size current > size largest = [current, smallest]
            | otherwise = [largest, smallest]
        loop [last] current             
            | size current < size last = [current, last]
            | otherwise = [last, current]
        loop arr current = current : arr
