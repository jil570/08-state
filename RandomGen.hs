{-# LANGUAGE ScopedTypeVariables #-}

module RandomGen where

-- make sure you have filled in all of the 'undefined' values
-- in the State module.

import Control.Monad
import State
import System.Random (StdGen, next, randomIO)
import qualified System.Random as Random (mkStdGen)

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

testRandom :: Int -> Int
testRandom i = fst (next (mkStdGen i))

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = undefined

-- test `nextBounded` with 20 randomly generated numbers, using the bound n
testBounded :: Int -> Bool
testBounded n = all (\x -> x >= 0 && x < n) (g n <$> [0 .. 20])
  where
    g x = fst . nextBounded x . mkStdGen

-- | Extract random values of any type
class Arb1 a where
  arb1 :: StdGen -> (a, StdGen)

instance Arb1 Int where
  arb1 = next

instance Arb1 Bool where
  arb1 = undefined

testArb1 :: Arb1 a => Int -> a
testArb1 = fst . arb1 . mkStdGen

instance (Arb1 a, Arb1 b) => Arb1 (a, b) where
  arb1 = undefined

instance Arb1 a => Arb1 [a] where
  arb1 s = undefined

type Gen a = State StdGen a

class Arb a where
  arb :: Gen a

instance Arb Int where
  arb = do
    s <- get
    let (y, s') = next s
    put s'
    return y

bounded :: Int -> Gen Int
bounded b = undefined

sample :: Show a => Gen a -> IO ()
sample gen = do
  seed <- (randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  undefined

instance (Arb a, Arb b) => Arb (a, b) where
  arb = undefined

elements :: [a] -> Gen a
elements = undefined

instance Arb Bool where
  arb = elements [False, True]

frequency :: [(Int, Gen a)] -> Gen a
frequency = undefined

instance (Arb a) => Arb [a] where
  arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]
