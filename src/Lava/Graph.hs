{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Lava.Graph (Lava, Graph(..),
                   computeGraph, mkEdge, mkNode)
where
import Control.Monad.State.Lazy ( execState, State , get, put)

data Graph t nodeType = Graph {
  nodeCount :: Int,
  netCount ::  Int,
  graphData :: t,
  nodes :: [(Int, nodeType)]
}

type Lava t nodeType = State (Graph t nodeType)

initialState :: t ->  Graph t nodeType
initialState initVal = Graph 0 0 initVal []

computeGraph :: t -> Lava t nodeType a -> Graph t nodeType
computeGraph initData nl = execState nl (initialState initData)

mkEdge :: Lava t nodeType Int
mkEdge = do graph <- get
            let nc = netCount graph
            put (graph{netCount = nc +1})
            return nc

mkNode :: nodeType -> Lava t nodeType ()
mkNode nodeValue
  = do n <- mkEdge
       graph <- get
       let nodesList = nodes graph
       put (graph{nodes = (n, nodeValue):nodesList})
