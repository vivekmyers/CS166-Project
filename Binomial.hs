module Binomial (
    PriorityQueue, new, add, poll, merge, decrease, addAll, toList, isEmpty
    ) where

-- |Contains binomial queue of added elements and binomial queue of deleted ones
data PriorityQueue e a = PQ {added :: [Tree e a], deleted :: [Tree e a]} deriving Show

-- |Constructs an empty priority queue with elements e and ordering a in O(1) time
new :: (Eq e, Ord a) => PriorityQueue e a
new = PQ [] []

-- |Gets if the queue is empty in O(1) time
isEmpty :: (Eq e, Ord a) => PriorityQueue e a -> Bool
isEmpty PQ {added = []} = True
isEmpty _ = False 

-- |Add x with priority p to pq in O(1) time
add :: (Eq e, Ord a) => e -> a -> PriorityQueue e a -> PriorityQueue e a
add x p pq = pq {added = insert x p $ added pq}

{-
    |Return the minimum element and the remaining queue after 
polling in O(log n) amortized time
-}
poll :: (Eq e, Ord a) => PriorityQueue e a -> (e, PriorityQueue e a)
poll PQ {added = []} = empty
poll pq = (first, cleanup . PQ rest $ deleted pq)
    where first = fst . findMin . added $ pq
          rest = deleteMin . added $ pq

-- |Meld two priority queues in O(log n) time
merge :: (Eq e, Ord a) => PriorityQueue e a -> PriorityQueue e a -> PriorityQueue e a
merge a b = PQ (meld (added a) (added b)) (meld (deleted a) (deleted b))

-- |Decrease the priority of x from p to q in pq in O(1) amortized
decrease :: (Eq e, Ord a) => e -> a -> a -> PriorityQueue e a -> PriorityQueue e a 
decrease x p q pq
    | q < p = cleanup $ PQ (insert x q $ added pq) (insert x p $ deleted pq)
    | otherwise = error "must decrease key"

-- |Adds a list of m (element, priority) pairs to the given priority queue in O(m) time
addAll :: (Eq e, Ord a) => [(e, a)] -> PriorityQueue e a -> PriorityQueue e a
addAll list pq = foldr (uncurry add) pq list

-- |Converts the priority queue to a sorted list in O(n log n) time
toList :: (Eq e, Ord a) => PriorityQueue e a -> [e]
toList pq
    | isEmpty pq = []
    | otherwise = x : toList rest
    where (x, rest) = poll pq

-- |Removes tombstones
cleanup pq@PQ {added = []} = pq
cleanup pq@PQ {deleted = []} = pq
cleanup pq@PQ {added = a, deleted = d}
    | findMin a == findMin d = cleanup $ PQ (deleteMin a) (deleteMin d)
    | otherwise = pq


-- |Binomial tree of nodes with root, priority, and tree rank
data Tree e a = Node {
    root :: e, 
    priority :: a, 
    rank :: Integer, 
    children :: [Tree e a]
    } deriving Show

-- helper functions for operations on lists of skew binomial trees below

link t1@(Node e1 a1 r1 c1) t2@(Node e2 a2 r2 c2)
    | a1 > a2 = Node e2 a2 (r2 + 1) $ t1 : c2
    | otherwise = Node e1 a1 (r1 + 1) $ t2 : c1

ins t [] = [t]
ins t (t':ts)
    | rank t < rank t' = t : t' : ts
    | otherwise = flip ins ts $ link t t'

insert x p ts = ins (Node x p 0 []) ts

meld [] ts = ts
meld ts [] = ts
meld (t1:ts1) (t2:ts2)
    | rank t1 < rank t2 = t1 : meld ts1 (t2 : ts2)
    | rank t2 < rank t1 = t2 : meld (t1 : ts1) ts2
    | otherwise = ins (link t1 t2) (meld ts1 ts2)

empty = error "empty queue"

findMin [] = empty
findMin [t] = (root t, priority t)
findMin (t:ts)
    | priority t <= p = (root t, priority t)
    | otherwise = (x, p)
    where (x, p) = findMin ts
    
deleteMin [] = empty
deleteMin ts = flip meld ts'' $ reverse c
    where 
        getMin [t] = (t, [])
        getMin (t:ts)
            | priority t <= priority t' = (t, ts)
            | otherwise = (t', t:ts')
            where (t', ts') = getMin ts
        (Node x p r c, ts'') = getMin ts

