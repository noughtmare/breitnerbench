{-# LANGUAGE DeriveFoldable #-}

-- We export all the implementations to prevent some optimizations that happen
-- to non-exported functions.
module Main
  ( main
  , isOrdered1
  , isOrdered2
  , isOrdered3
  , isOrdered4
  , isOrdered5
  , isOrdered5'
  , isOrdered5''
  , isOrdered5'''
  , isOrdered6
  , isOrdered7
  , isOrdered8
  , isOrdered9
  , isOrdered10
  , isOrdered11
  , isOrdered12
  , isOrdered13
  ) where

import Data.Foldable
import Data.Maybe
import Control.Monad
import Criterion.Main

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Foldable)

isOrdered1 :: Ord a => Tree a -> Bool
isOrdered1 = everynode (\l x r -> all (<= x) (elems l) && all (>= x) (elems r))

everynode :: (Tree a -> a -> Tree a -> Bool) -> Tree a -> Bool
everynode p = go
  where
    go Leaf = True
    go (Node l x r) = p l x r && go l && go r

elems :: Tree a -> [a]
elems Leaf = []
elems (Node l x r) = elems l ++ [x] ++ elems r

isOrdered2 :: Ord a => Tree a -> Bool
isOrdered2 = go
  where
    go Leaf = True
    go (Node l x r) =
        all (<= x) (elems l) &&
        all (>= x) (elems r) &&
        go l &&
        go r

allElem p Leaf = True
allElem p (Node l x r) = allElem p l && p x && allElem p r

isOrdered3 :: Ord a => Tree a -> Bool
isOrdered3 = go
  where
    go Leaf = True
    go (Node l x r) =
      allElem (<= x) l && go l &&
      allElem (>= x) r && go r

isOrdered4 :: Ord a => Tree a -> Bool
isOrdered4 = go' (const True)
  where
    go' :: Ord a => (a -> Bool) -> Tree a -> Bool
    go' p Leaf = True
    go' p (Node l x r) =
        p x &&
        go' (\y -> p y && y <= x) l &&
        go' (\y -> p y && y >= x) r

isOrdered5 :: Ord a => Tree a -> Bool
isOrdered5 = go' (Nothing, Nothing)
  where
    go' :: Ord a => (Maybe a, Maybe a) -> Tree a -> Bool
    go' p Leaf = True
    go' (lb, ub) (Node l x r) =
        maybe True (<= x) lb &&
        maybe True (>= x) ub &&
        go' (lb, Just x) l &&
        go' (Just x, ub) r

isOrdered5' :: Tree Int -> Bool
isOrdered5' = go' (Nothing, Nothing)
  where
    go' :: (Maybe Int, Maybe Int) -> Tree Int -> Bool
    go' p Leaf = True
    go' (lb, ub) (Node l x r) =
        maybe True (<= x) lb &&
        maybe True (>= x) ub &&
        go' (lb, Just x) l &&
        go' (Just x, ub) r

isOrdered5'' :: Tree Int -> Bool
isOrdered5'' = go' Nothing Nothing
  where
    go' :: Maybe Int -> Maybe Int -> Tree Int -> Bool
    go' _ _ Leaf = True
    go' lb ub (Node l x r) =
        maybe True (<= x) lb &&
        maybe True (>= x) ub &&
        go' lb (Just x) l &&
        go' (Just x) ub r

isOrdered5''' :: Tree Int -> Bool
isOrdered5''' = go' minBound maxBound
  where
    go' :: Int -> Int -> Tree Int -> Bool
    go' _ _ Leaf = True
    go' lb ub (Node l x r) = lb <= x && ub >= x && go' lb x l && go' x ub r

isOrdered6 :: Ord a => Tree a -> Bool
isOrdered6 t = fst (go' t)
  where
    go' Leaf = (True, [])
    go' (Node l x r) =
      let (go_l, elems_l) = go' l in
      let (go_r, elems_r) = go' r in
      ( all (<= x) elems_l &&
        all (>= x) elems_r &&
        go_l &&
        go_r
      , elems_l ++ [x] ++ elems_r
      )

isOrdered7 :: Ord a => Tree a -> Bool
isOrdered7 t = isJust (go' t)
  where
    go' :: Ord a => Tree a -> Maybe [a]
    go' Leaf = Just []
    go' (Node l x r) = do
      elems_l <- go' l
      elems_r <- go' r
      guard $ all (<= x) elems_l
      guard $ all (>= x) elems_r
      return $ elems_l ++ [x] ++ elems_r

isOrdered8 :: Ord a => Tree a -> Bool
isOrdered8 t = isJust (go' t)
  where
    go' :: Ord a => Tree a -> Maybe [a]
    go' Leaf = Just []
    go' (Node l x r) = do
      elems_l <- go' l
      elems_r <- go' r
      guard $ null elems_l || maximum elems_l <= x
      guard $ null elems_r || minimum elems_r >= x
      return $ elems_l ++ [x] ++ elems_r

isOrdered9 :: Ord a => Tree a -> Bool
isOrdered9 t = isJust (go' t)
  where
    go' :: Ord a => Tree a -> Maybe [a]
    go' Leaf = Just []
    go' (Node l x r) = do
      elems_l <- go' l
      elems_r <- go' r
      guard $ null elems_l || last elems_l <= x
      guard $ null elems_r || head elems_r >= x
      return $ elems_l ++ [x] ++ elems_r

isOrdered10 :: Ord a => Tree a -> Bool
isOrdered10 t = isJust (go' t)
  where
    go' :: Ord a => Tree a -> Maybe (Maybe (a, a))
    go' Leaf = Just Nothing
    go' (Node l x r) = do
      elems_l <- go' l
      elems_r <- go' r
      for_ elems_l $ \(l, u) -> guard $ u <= x
      for_ elems_r $ \(l, u) -> guard $ l >= x
      return $ elems_l <.> Just (x, x) <.> elems_r

    Nothing <.> x = x
    x <.> Nothing = x
    Just (l,_) <.> Just (_,r) = Just (l, r)

isOrdered11 :: Ord a => Tree a -> Bool
isOrdered11 t = sortedList (go' t)
  where
    go' :: Tree a -> [a]
    go' Leaf = []
    go' (Node l x r) = go' l ++ [x] ++ go' r

sortedList :: Ord a => [a] -> Bool
sortedList [] = True
sortedList [x] = True
sortedList (x:y:zs) = x <= y && sortedList (y:zs)

isOrdered12 :: Ord a => Tree a -> Bool
isOrdered12 t = sortedList (go' t [])
  where
    go' :: Tree a -> ([a] -> [a])
    go' Leaf = id
    go' (Node l x r) = go' l . (x:) . go' r

isOrdered13 :: Ord a => Tree a -> Bool
isOrdered13 t = sortedList (toList t)

main :: IO ()
main = defaultMain
  [ bgroup "isOrdered1"
    [ bench "10000" $ whnf isOrdered1 t4
    ]
  , bgroup "isOrdered2"
    [ bench "10000" $ whnf isOrdered2 t4
    ]
  , bgroup "isOrdered3"
    [ bench "10000" $ whnf isOrdered3 t4
    ]
  , bgroup "isOrdered4"
    [ bench "10000" $ whnf isOrdered4 t4
    ]
  , bgroup "isOrdered5"
    [ bench "10000" $ whnf isOrdered5 t4
    ]
  , bgroup "isOrdered5 (specialized)"
    [ bench "10000" $ whnf isOrdered5' t4
    ]
  , bgroup "isOrdered5 (curried)"
    [ bench "10000" $ whnf isOrdered5'' t4
    ]
  , bgroup "isOrdered5 (bounded)"
    [ bench "10000" $ whnf isOrdered5''' t4
    ]
  , bgroup "isOrdered6"
    [ bench "10000" $ whnf isOrdered6 t4
    ]
  , bgroup "isOrdered7"
    [ bench "10000" $ whnf isOrdered7 t4
    ]
  , bgroup "isOrdered8"
    [ bench "10000" $ whnf isOrdered8 t4
    ]
  , bgroup "isOrdered9"
    [ bench "10000" $ whnf isOrdered9 t4
    ]
  , bgroup "isOrdered10"
    [ bench "10000" $ whnf isOrdered10 t4
    ]
  , bgroup "isOrdered11"
    [ bench "10000" $ whnf isOrdered11 t4
    ]
  , bgroup "isOrdered12"
    [ bench "10000" $ whnf isOrdered12 t4
    ]
  , bgroup "isOrdered13"
    [ bench "10000" $ whnf isOrdered13 t4
    ]
  ]
  where
    t4 = mkBalancedOrderedTree 10000

    mkBalancedOrderedTree :: Int -> Tree Int
    mkBalancedOrderedTree n = go (0, n) where
      go (l, u)
        | l == u = Leaf
        | otherwise = Node (go (l, m)) m (go (m + 1, u))
        where
          m = (l + u) `quot` 2
