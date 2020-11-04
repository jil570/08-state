{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module StateMonads where

import Control.Monad (ap, liftM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import State

-- import Control.Monad.State

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

tree :: Tree Char
tree = Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

countF :: Tree a -> Int
countF (Leaf _) = 1
countF (Branch t1 t2) = countF t1 + countF t2

-- | The number of leaves in the tree that we have currently counted
type Store = Int

countI :: Tree a -> Int
countI t = aux t 0
  where
    aux :: Tree a -> (Store -> Store)
    aux (Leaf _) = (+ 1)
    aux (Branch t1 t2) = \s ->
      let s' = aux t1 s
          s'' = aux t2 s'
       in s''

label1 :: Tree a -> Tree (a, Int)
label1 t = fst (aux t 0)
  where
    aux :: Tree a -> Store -> (Tree (a, Int), Store)
    aux (Leaf x) s = (Leaf (x, s), s + 1)
    aux (Branch t1 t2) s =
      let (t', s') = aux t1 s
          (t'', s'') = aux t2 s'
       in (Branch t' t'', s'')

--     SPOILER SPACE BELOW
--
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |
--      |

label1' :: Tree a -> Tree (a, Int)
label1' t = fst (aux t 0)
  where
    aux :: Tree a -> Store -> (Tree (a, Int), Store)
    aux (Leaf x) s = (Leaf (x, s), s + 1)
    aux (Branch t1 t2) s =
      let (t1', s') = aux t1 s
          (t2', s'') = aux t2 s'
       in (Branch t1' t2', s'')

type ST a = Store -> (a, Store)

returnST :: a -> ST a
returnST x s = (x, s)

-- (store->(a,store)) -> (a->(store->(b,store))) -> (store -> (b,store))
bindST :: ST a -> (a -> ST b) -> ST b
bindST f g = \s -> let (a, s') = f s in g a s'

label2 :: Tree a -> Tree (a, Int)
label2 t = fst (aux t 0)
  where
    aux :: Tree a -> ST (Tree (a, Int))
    aux (Leaf x) = \s -> (Leaf (x, s), s + 1)
    aux (Branch t1 t2) =
      bindST
        (aux t1)
        ( \t1' ->
            bindST
              (aux t2)
              (\t2' -> returnST (Branch t1' t2'))
        )

newtype ST2 a = S {apply :: Store -> (a, Store)}

instance Monad ST2 where
  return :: a -> ST2 a
  return x = S (x,) -- this tuple section (x,) is equivalent to \y -> (x,y)

  (>>=) :: ST2 a -> (a -> ST2 b) -> ST2 b
  f >>= g = S $ \s ->
    let (a, s') = apply f s
     in apply (g a) s'

instance Functor ST2 where
  fmap = liftM

instance Applicative ST2 where
  pure = return
  (<*>) = ap

fresh :: ST2 Int
fresh = S $ \s -> (s, s + 1)

mlabel :: Tree a -> ST2 (Tree (a, Int))
mlabel (Leaf x) = do
  y <- fresh
  return (Leaf (x, y))
mlabel (Branch t1 t2) = do
  t1' <- mlabel t1
  t2' <- mlabel t2
  return (Branch t1' t2')

label :: Tree a -> Tree (a, Int)
label t = fst (apply (mlabel t) 0)

freshS :: State Int Int
freshS = undefined

mlabelS :: Tree t -> State Int (Tree (t, Int))
mlabelS (Leaf x) = do
  y <- freshS
  return (Leaf (x, y))
mlabelS (Branch t1 t2) = do
  t1' <- mlabelS t1
  t2' <- mlabelS t2
  return (Branch t1' t2')

data MySt a = M
  { index :: Int,
    freq :: Map a Int
  }
  deriving (Eq, Show)

freshM :: State (MySt a) Int
freshM = do
  m <- get
  let i = index m
  put (M (i + 1) (freq m))
  return i

updFreqM :: Ord a => a -> State (MySt a) ()
updFreqM = undefined

mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
mlabelM (Leaf x) = do
  y <- freshM
  updFreqM x
  return (Leaf (x, y))
mlabelM (Branch t1 t2) = do
  t1' <- mlabelM t1
  t2' <- mlabelM t2
  return (Branch t1' t2')

initM :: MySt a
initM = M 0 Map.empty
