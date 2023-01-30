module Test.MySolutions
  ( filterM
  , possibleSums
  , simulate
  )
  where

import Data.List
import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.RWS (modify)
import Control.Monad.ST (ST, for)
import Control.Monad.ST.Internal (new, read)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Random (random)
import Node.Encoding (Encoding(..))
import Node.FS.Async (readTextFile)

-- Note to reader: Add your solutions to this file

third :: forall a. Array a -> Maybe a
third arr = do
  skip1st <- A.tail arr
  skip2nd <- A.tail skip1st
  A.head skip2nd

possibleSums :: Array Int -> Array Int
possibleSums xs = A.nub $ A.sort $ A.foldM (\acc i -> [ acc, acc + i ]) 0 xs

filterM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  b <- f x
  xs' <- filterM f xs
  pure if b then x : xs' else xs'

numberGen :: Effect Unit
numberGen = do
    n <- random
    logShow n

readTextFile2 :: Effect Unit
readTextFile2 = do
    result <- try $ readTextFile UTF8 "OMG.md"
    case result of
        Right lines -> log $ lines
        Left err -> log err

simulate :: forall r. Number -> Number -> Int -> ST r Number
simulate x0 v0 time = do
    ref <- new { x: x0, v: v0 }
    for 0 (time * 1000) \_ ->
        modify (\o -> {v: o.v - 9.81 * 0.001, x: o.x + o.v * 0.001 }) ref
    final <- read ref
    pure final.x