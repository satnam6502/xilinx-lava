{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Lava.Graph (Lava, Graph(..),
                   computeGraph, mkNewNodeNumber, mkNewEdgeNumber, mkNode)
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

mkNewNodeNumber :: Lava t nodeType Int
mkNewNodeNumber
  = do graph <- get
       let nc = nodeCount graph
       put (graph{nodeCount = nc +1})
       return nc

mkNode :: nodeType -> Lava t nodeType ()
mkNode nodeValue
  = do n <- mkNewNodeNumber
       graph <- get
       let nodesList = nodes graph
       put (graph{nodes = (n, nodeValue):nodesList})


mkNewEdgeNumber :: Lava t nodeType Int
mkNewEdgeNumber
  = do graph <- get
       let ec = netCount graph
       put (graph{netCount = ec +1})
       return ec

        