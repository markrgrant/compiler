-- a Set implementation using ordered lists

module Set (
    Set,
    empty,
    sing,
    member,
    union, intersection, diff,
    subset,
    fromList,
    toList,
    filter,
    card,
    setlimit
) where

import Prelude as P hiding (filter)
import Data.List hiding ( union, filter )
import Data.Monoid
import Data.Functor

-- a set is implemented as a List of ordered elements with no duplicates
newtype Set a = Set [a] deriving (Ord)

-- return an empty Set
empty :: Set a
empty = Set []

-- return a Set with one element
sing :: a -> Set a
sing x = Set [x]

-- return a Set containing the elements in the List
fromList :: Ord a => [a] -> Set a
fromList lst = Set ((dedup . sort) lst)
    where
        dedup [] = []
        dedup [x] = [x]
        dedup (x:y:ys)
            | x == y = y:(dedup ys)
            | otherwise = x:(dedup (y:ys))

-- return True if the value is a member of the set, False otherwise
member :: (Eq a) => a -> Set a -> Bool
member x (Set lst) = x `elem` lst

-- return a Set containing the members that are in either s1 or
-- s2 or both
union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set (uni s1 s2)

uni xs [] = xs
uni [] ys = ys
uni (x:xs) (y:ys)
  | x == y = x:(uni xs ys)
  | x < y = x:(uni xs (y:ys))
  | x > y = y:(uni (x:xs) ys)


-- return a set containing the members that are in both s1 and s2
intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set s1) (Set s2) = Set (inter s1 s2)
inter xs [] = []
inter [] xs = []
inter (x:xs) (y:ys)
    | x == y = x:(inter xs ys)
    | x < y = inter xs (y:ys)
    | x > y = inter (x:xs) ys

-- return a set containing the members in s1 that are not in s2
diff :: Ord a => Set a -> Set a -> Set a
diff (Set s1) (Set s2) = Set (dif s1 s2)
dif [] xs = []
dif xs [] = xs
dif (x:xs) (y:ys)
    | x == y = dif xs ys
    | x < y = x:(dif xs  (y:ys))
    | x > y = dif (x:xs) ys

-- return True if s2 is a subset of s1, False otherwise
subset :: Ord a => Set a -> Set a -> Bool
subset (Set _) (Set []) = True
subset (Set []) (Set _) = False 
subset (Set (x:xs)) (Set (y:ys))
    | x == y = subset (Set xs) (Set ys)
    | x < y = subset (Set xs) (Set (y:ys))
    | otherwise = False

-- Return a List containing the members of the Set.
toList :: Set a -> [a]
toList (Set xs) = xs

-- Return the number of members in the Set.
card :: Set a -> Int
card (Set xs) = length xs

-- repeatedly apply a function to the set.  When applying
-- the function no longer changes the set, return
-- that set.
setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
    | s == next = s
    | otherwise = setlimit f next
    where next = f s

-- remove members from the Set based on a filter function
filter :: (Eq a) => (a -> Bool) -> Set a -> Set a
filter func (Set []) = (Set [])
filter func (Set lst) = (Set (lfilter func lst))
    where lfilter func [] = []
          lfilter func (x:xs)
            | (func x) = x:(lfilter func xs)
            | otherwise = lfilter func xs

instance (Eq a) => Eq (Set a) where
    (==) (Set xs) (Set ys) = xs == ys

instance (Show a) => Show (Set a) where
    show (Set xs) = show xs

instance Functor Set where
    fmap f (Set lst) = Set (fmap f lst)

instance (Ord a) => Monoid (Set a) where
    mempty = empty
    mappend s1 s2 = union s1 s2

-- Monad laws:
--   'return x >>= f' is the same as 'f x'
--   'm >>= return' is the same as  m'
--   '(m >>= f) >>= g' is the same as 'm >>= (\x -> f x >>= g)'
instance Monad Set where
    return val = sing val

