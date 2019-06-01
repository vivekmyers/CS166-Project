module PriorityQueue (
    PriorityQueue, new, add, poll, merge, decrease
    ) where

data PriorityQueue e a = PQ {added :: BPQ e a, deleted :: BPQ e a} deriving Show

cleanup pq@PQ {added = Empty} = pq
cleanup pq@PQ {deleted = Empty} = pq
cleanup pq@PQ {added = a, deleted = d}
    | findMin' a == findMin' d && findP' a == findP' d = cleanup $ PQ (deleteMin' a) (deleteMin' d)
    | otherwise = pq

new :: (Eq e, Ord a) => PriorityQueue e a
new = PQ Empty Empty

add :: (Eq e, Ord a) => e -> a -> PriorityQueue e a -> PriorityQueue e a
add x p pq = pq {added = insert' x p $ added pq}

poll :: (Eq e, Ord a) => PriorityQueue e a -> (e, PriorityQueue e a)
poll PQ {added = Empty} = empty
poll pq = (first, cleanup . PQ rest $ deleted pq)
    where first = findMin' . added $ pq
          rest = deleteMin' . added $ pq

merge :: (Eq e, Ord a) => PriorityQueue e a -> PriorityQueue e a -> PriorityQueue e a
merge a b = PQ (meld' (added a) (added b)) (meld' (deleted a) (deleted b))

decrease :: (Eq e, Ord a) => e -> a -> a -> PriorityQueue e a -> PriorityQueue e a 
decrease x p q pq
    | q < p = cleanup $ PQ (insert' x q $ added pq) (insert' x p $ deleted pq)
    | otherwise = error "must decrease key"

data BPQ e a = Empty | BPQ (e, a) [Tree (BPQ e a) a] deriving Show

findMin' Empty = empty
findMin' (BPQ (e, a) _) = e

findP' Empty = empty
findP' (BPQ (e, a) _) = a

insert' x p q = meld' (BPQ (x, p) []) q

meld' a Empty = a
meld' Empty b = b
meld' (BPQ x1@(_, p1) q1) (BPQ x2@(_, p2) q2)
    | p1 <= p2 = BPQ x1 $ insert (BPQ x2 q2) p2 q1
    | otherwise = BPQ x2 $ insert (BPQ x1 q1) p1 q2
    
deleteMin' (BPQ _ []) = Empty
deleteMin' (BPQ x q) = BPQ y $ meld q1 q2
    where BPQ y q1 = fst . findMin $ q
          q2 = deleteMin q

data Tree e a = Node {
    root :: e, 
    priority :: a, 
    rank :: Integer, 
    children :: [Tree e a]
    } deriving Show

link (Node e1 a1 r1 c1) (Node e2 a2 r2 c2)
    | a1 > a2 = Node e2 a2 (r2 + 1) $ Node e1 a1 r1 c1 : c2
    | otherwise = Node e1 a1 (r1 + 1) $ Node e2 a2 r2 c2 : c1

skewLink t0@(Node e0 a0 r0 c0) t1@(Node e1 a1 r1 c1) t2@(Node e2 a2 r2 c2) 
    | a1 <= a0 && a1 <= a2 = Node e1 a1 (r1 + 1) $ t0 : t2 : c1
    | a2 <= a0 && a2 <= a1 = Node e2 a2 (r2 + 1) $ t0 : t1 : c2
    | otherwise = Node e0 a0 (r1 + 1) [t1, t2]

ins t [] = [t]
ins t (t':ts)
    | rank t < rank t' = t : t' : ts
    | otherwise = flip ins ts $ link t t'

uniquify [] = []
uniquify (t:ts) = ins t ts

meldUniq [] ts = ts
meldUniq ts [] = ts
meldUniq (t1:ts1) (t2:ts2)
    | rank t1 < rank t2 = t1 : meldUniq ts1 (t2 : ts2)
    | rank t2 < rank t1 = t2 : meldUniq (t1:ts1) ts2
    | otherwise = ins (link t1 t2) $ meldUniq ts1 ts2

insert x p ts@(t1:t2:rest)
    | rank t1 == rank t2 = skewLink (Node x p 0 []) t1 t2 : rest
    | otherwise = Node x p 0 [] : ts
insert x p ts = Node x p 0 [] : ts

meld ts ts' = meldUniq (uniquify ts) $ uniquify ts'

empty = error "empty queue"

findMin [] = empty
findMin [t] = (root t, priority t)
findMin (t:ts)
    | priority t < p = (root t, priority t)
    | otherwise = (x, p)
    where (x, p) = findMin ts
    
deleteMin [] = empty
deleteMin ts = foldr (uncurry insert) (meld ts'' ts') xs'
    where 
        getMin [t] = (t, [])
        getMin (t:ts)
            | priority t <= priority t' = (t, ts)
            | otherwise = (t', t:ts')
            where (t', ts') = getMin ts
        split ts xs [] = (ts, xs)
        split ts xs (t:c)
            | rank t == 0 = split ts ((root t, priority t) : xs) c
            | otherwise = split (t : ts) xs c
        (Node x p r c, ts'') = getMin ts
        (ts', xs') = split [] [] c


