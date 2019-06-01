import PriorityQueue

main = do
    let pq1 = foldr (add <*> id) (new :: PriorityQueue Int Int) [1..10]
    let pq2 = foldr (add <*> id) (new :: PriorityQueue Int Int) [(-1)..4]
    putStr "First Element: "
    print . fst.poll . (merge pq2) . snd.poll . decrease 3 3 0 $ pq1
    putStr "Queue: "
    print . snd.poll . (merge pq2) . snd.poll . decrease 3 3 0 $ pq1
