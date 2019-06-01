import PriorityQueue
import System.Random
import Control.Monad
import Data.Tree


main = do
    let pq1 = addAll [(2*i, -2*i) | i <- [1..10]] new
        pq2 = addAll [(2*i+1, -2*i-1) | i <- [1..10]] new
    print $ toList $ merge pq1 pq2
