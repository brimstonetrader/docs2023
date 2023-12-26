> import Data.Map (Map, (!))
> import Data.Map qualified as M
> import Data.Sequence qualified as Seq
> import Data.Set (Set)
> import Data.Set qualified as S

> main = interact $ unlines . findPath "Brent" "END" . readGraph 

> type Vertex = String
> type Graph = Map Vertex [Vertex]

> readGraph :: String -> Graph
> readGraph = M.fromListWith (++) . concatMap (mkEdges . words) . drop 1 . lines
>  where
>   mkEdges [u, v] = [(u, [v]), (v, [u])]

> findPath :: Vertex -> Vertex -> Graph -> [Vertex]
> findPath start end g = bfs start end (g !)

> bfs :: (Ord a) => a -> a -> (a -> [a]) -> [a]
> bfs start goal next = reverse (tracePath goal (bfs' S.empty M.empty (Seq.singleton start)))
>  where
>   bfs' !seen !parent !queue = case Seq.viewl queue of
>     Seq.EmptyL -> parent
>     u Seq.:< queue' ->
>       let newNeighbors = filter (`S.notMember` seen) (next u)
>        in bfs'
>             (seen `S.union` S.fromList newNeighbors)
>             (parent `M.union` M.fromList (map (,u) newNeighbors))
>             (queue' Seq.>< Seq.fromList newNeighbors)
> 
>   tracePath cur parent
>     | cur == start = [cur]
>     | otherwise = cur : tracePath (parent ! cur) parent
