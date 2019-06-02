import qualified Data.Map as Map
import Data.Map (Map, (!))
import qualified Data.Set as Set
import Data.Set (Set)
import PriorityQueue
import qualified Binomial as B
import System.Random
import Criterion.Main

skewHeapsort xs = toList $ addAll (zip xs xs) new
binHeapsort xs = B.toList $ B.addAll (zip xs xs) B.new

skewMeld nums = toList . foldr f new $ 
                    do num <- chunk (2 * n) nums 
                       let queue = addAll (zip <*> id $ num) new
                       return queue
    where n = floor $ fromIntegral (length nums) / 100
          f pq1 pq2 = iterate (snd.poll) (merge pq1 pq2) !! n
binMeld nums = B.toList . foldr f B.new $ 
                    do num <- chunk (2 * n) nums 
                       let queue = B.addAll (zip <*> id $ num) B.new
                       return queue
    where n = floor $ fromIntegral (length nums) / 100
          f pq1 pq2 = iterate (snd.B.poll) (B.merge pq1 pq2) !! n

skewDijkstra :: [Integer] -> Int -> [Integer] 
skewDijkstra nums deg = toList . fst $ foldr f (addAll [(x, m) | x <- nums] new, 
        Map.fromList [(i, m) | i <- nums]) (chunk deg nums)
    where f near (pq, weights) = (foldr (\x q -> let i = nw ! x
                                                 in if i < 0 then q
                                                    else decrease x i (i - 1) q) rest near, 
                    foldr (flip (Map.insertWith (+)) (-1)) nw near)        
                where (popped, rest) = poll pq
                      nw = Map.insert popped (-1) weights    
          m = floor . exp $ 9 * log 10
binDijkstra :: [Integer] -> Int -> [Integer] 
binDijkstra nums deg = B.toList . fst $ foldr f (B.addAll [(x, m) | x <- nums] B.new, 
        Map.fromList [(i, m) | i <- nums]) (chunk deg nums)
    where f near (pq, weights) = (foldr (\x q -> let i = nw ! x
                                                 in if i < 0 then q
                                                    else B.decrease x i (i - 1) q) rest near, 
                    foldr (flip (Map.insertWith (+)) (-1)) nw near)        
                where (popped, rest) = B.poll pq
                      nw = Map.insert popped (-1) weights    
          m = floor . exp $ 9 * log 10


chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

run n = do
    let max = floor . exp $ n * log 10 :: Integer
    nums <- take (fromIntegral max) <$> (randomRs (1, 1000 * max) <$> getStdGen) :: IO [Integer]
    let benchn x = bench $ x ++ "-" ++ show max
    return [ 
            bgroup "binomial" [ 
                benchn "heapsort" $ whnf binHeapsort nums,
                benchn "meld" $ whnf binMeld nums,
                benchn "dijkstra" $ whnf (binDijkstra nums) 4
                ],
            bgroup "skew" [ 
                benchn "heapsort" $ whnf skewHeapsort nums,
                benchn "meld" $ whnf skewMeld nums,
                benchn "dijkstra" $ whnf (skewDijkstra nums) 4
                ]
        ]

main = do
    result <- mapM run [4]
    defaultMain $ concat result
